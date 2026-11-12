(ns bot.dry-run
  (:require ["fs/promises" :as fs]
            [bot.ai :as ai]
            [bot.beu :as beu]
            [bot.date :as date]
            [bot.pdf :as pdf]
            [bot.image-focus :as focus]
            [bot.post-draft :as draft]
            [bot.report-preview :as preview]
            [bot.source-sections :as sections]
            [bot.source-preview :as source-preview]
            [clojure.string :as str]))

(def help-text
  (str "AI preview dry run — never publishes or accesses social-network accounts.\n\n"
       "npm run build:dry-run\n"
       "OPENAI_API_KEY=... npm run dry-run -- [options]\n\n"
       "  --count N        Latest N reports, default 1 (maximum 20)\n"
       "  --url URL        One BEU report detail page or PDF URL\n"
       "  --manifest FILE  JSON array of report metadata, optionally with pdf-path\n"
       "  --out DIR        Output directory, default reports/ai-preview\n"
       "  --model NAME     OpenAI vision model, default gpt-4.1-mini\n"
       "  --refresh        Regenerate even if a matching cached draft exists\n"
       "  --help           Show this help\n\n"
       "Run npm run setup:preview once for font repair. Text is copied programmatically;\n"
       "AI selects images and only shortens text when necessary.\n"
       "Model can also be set with OPENAI_MODEL. Mastodon limits are read publicly\n"
       "from MASTO_BASE_URI (default https://zug.network). No access token needed.\n"))

(defn parse-args [args]
  (loop [remaining args
         opts {:count 1 :out "reports/ai-preview"
               :model (or (.. js/process -env -OPENAI_MODEL) ai/default-model)}]
    (if-let [arg (first remaining)]
      (cond
        (= arg "--help") (assoc opts :help? true)
        (= arg "--refresh") (recur (rest remaining) (assoc opts :refresh? true))
        (contains? #{"--count" "--out" "--model" "--manifest" "--url"} arg)
        (let [value (second remaining)]
          (when (or (str/blank? value) (str/starts-with? value "--"))
            (throw (ex-info (str "Missing value for " arg) {})))
          (recur (drop 2 remaining)
                 (assoc opts (keyword (subs arg 2))
                        (if (= arg "--count")
                          (let [n (js/Number value)]
                            (when-not (and (integer? n) (<= 1 n 20))
                              (throw (ex-info "--count must be an integer from 1 to 20" {})))
                            n)
                          value))))
        :else (throw (ex-info (str "Unknown option: " arg) {})))
      (do
        (when (and (:url opts) (:manifest opts))
          (throw (ex-info "Use either --url or --manifest" {})))
        opts))))

(defn validate-report [report]
  (when-not (and (re-matches #"\[id:[a-zA-Z0-9_-]{10}\]" (or (:report-id report) ""))
                 (every? #(and (string? %) (not (str/blank? %)))
                         ((juxt :report-type :event-type :event-date :event-location :report-date) report))
                 (every? #(and (string? %) (re-matches #"https?://\S+" %))
                         ((juxt :report-overview-uri :report-pdf-uri) report)))
    (throw (ex-info "Invalid report metadata; see the README manifest example" {})))
  report)

(defn reports+ [{:keys [manifest count url]}]
  (cond
    url (-> (beu/report-from-url+ url) (.then #(vector (validate-report %))))
    manifest
    (-> (fs/readFile manifest "utf8")
        (.then (fn [text]
                 (let [reports (js->clj (.parse js/JSON text) :keywordize-keys true)]
                   (when-not (vector? reports)
                     (throw (ex-info "Manifest must be a JSON array" {})))
                   (mapv validate-report (take count reports))))))
    :else (-> (js/Promise.all [(beu/fetch-reports+ "Untersuchungsbericht")
                               (beu/fetch-reports+ "Zwischenbericht")])
              (.then #(->> % (mapcat identity)
                           (sort-by (fn [report] (date/german->iso (:report-date report))))
                           reverse (take count)))
              (.then beu/fetch-reports-details+)
              (.then #(mapv validate-report %)))))

(def json preview/json)
(def write-json+ preview/write-json+)
(def read-json-if-present+ preview/read-json-if-present+)

(defn escape-html [value]
  (-> (str value) (str/replace "&" "&amp;") (str/replace "<" "&lt;")
      (str/replace ">" "&gt;") (str/replace "\"" "&quot;")
      (str/replace "'" "&#39;")))

(defn focus-html [image-path point]
  (when (focus/valid? point)
    (str "<p>Mastodon focus: <code>" (escape-html (focus/mastodon point))
         "</code>. Example 16:9 crop (actual crop depends on the client):</p>"
         "<div class=\"focus-crop\" data-x=\"" (:x point) "\" data-y=\"" (:y point) "\">"
         "<img alt=\"Example crop around the selected focal point\" src=\""
         (escape-html (last (str/split image-path #"/")))
         "\" onload=\"previewFocus(this)\"></div>")))

(defn page-html [{:keys [page image-path text raw-text alt image-description reason roles focus]}]
  (str "<figure><a class=\"page-image\" href=\"" (escape-html (last (str/split image-path #"/")))
       "\"><img loading=\"lazy\" src=\"" (escape-html (last (str/split image-path #"/")))
       "\" alt=\"" (escape-html (or alt text)) "\">"
       (when (focus/valid? focus)
         (str "<span class=\"focus-marker\" title=\"Selected focus\" style=\"left:"
              (* 100 (:x focus)) "%;top:" (* 100 (:y focus)) "%\"></span>"))
       "</a><figcaption>PDF page " page
       (when reason (str " — " (escape-html reason)))
       (when (seq roles) (str " · " (escape-html (str/join ", " (map name roles))))) "</figcaption>"
       (focus-html image-path focus)
       (when-not (str/blank? image-description)
         (str "<p>Bildbeschreibung: " (escape-html image-description) "</p>"))
       "<details><summary>Full page text</summary><pre>" (escape-html text) "</pre></details>"
       (when raw-text (str "<details><summary>Original page text before repair</summary><pre>" (escape-html raw-text) "</pre></details>"))
       (when alt (str "<details><summary>Full image alt text</summary><pre>" (escape-html alt) "</pre></details>"))
       "</figure>"))

(defn post-html [platform post]
  (let [text (:text post)
        uri (:uri post)
        facet (first (:facets post))
        bytes (js/Buffer.from text "utf8")
        rendered (if facet
                   (let [{:keys [byteStart byteEnd]} (:index facet)]
                     (str (escape-html (.toString (.subarray bytes 0 byteStart) "utf8"))
                          "<a href=\"" (escape-html uri) "\">"
                          (escape-html (.toString (.subarray bytes byteStart byteEnd) "utf8")) "</a>"
                          (escape-html (.toString (.subarray bytes byteEnd) "utf8"))))
                   (str/replace (escape-html text) (escape-html uri)
                                (str "<a href=\"" (escape-html uri) "\">" (escape-html uri) "</a>")))]
    (str "<h3>" (escape-html (name platform)) "</h3><pre>"
         rendered "</pre><p class=\"" (if (:fits? post) "ok" "error") "\">"
         (:length post) " / " (:max-length post) " characters"
         (when (= platform :bluesky) (str "; " (:bytes post) " / 3000 UTF-8 bytes"))
         (if (:fits? post) " — fits" " — too long") "</p>"
         (when (seq (:dropped-tags post))
           (str "<p>Dropped tags: " (escape-html (str/join ", " (:dropped-tags post))) "</p>")))))

(defn comparison-html [{:keys [report baseline ai model cached? validation-errors source-sections raw-source-sections text-policy image-candidates proposed-image-candidates image-verifications]}]
  (str "<!doctype html><html lang=\"en\"><meta charset=\"utf-8\">"
       "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
       "<title>BEU preview comparison</title><style>"
       "body{font:16px system-ui;margin:2rem;background:#f6f7f8;color:#20252b}"
       "main{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:2rem}"
       "section{background:white;padding:1.5rem;border-radius:12px}"
       "pre{white-space:pre-wrap;overflow-wrap:anywhere;font:inherit;line-height:1.5}"
       "figure{margin:1rem 0}img{width:100%;max-height:620px;object-fit:contain;background:#eee}"
       ".page-image{display:block;position:relative;max-width:440px;margin:auto}"
       ".page-image img{display:block;max-height:none;height:auto}"
       ".focus-marker{position:absolute;transform:translate(-50%,-50%);width:24px;height:24px;border:3px solid white;border-radius:50%;box-shadow:0 0 0 2px #d21;pointer-events:none}"
       ".focus-crop{position:relative;aspect-ratio:16/9;overflow:hidden;background:#eee}"
       ".focus-crop img{position:absolute;width:auto;height:auto;max-width:none;max-height:none}"
       "figcaption{margin:.5rem 0}.ok{color:#176636}.error{color:#b02020}"
       "@media(max-width:800px){main{grid-template-columns:1fr}body{margin:1rem}}"
       "</style><script>function previewFocus(img){const box=img.parentElement;"
       "const draw=()=>{const w=box.clientWidth,h=box.clientHeight;"
       "const scale=Math.max(w/img.naturalWidth,h/img.naturalHeight);"
       "const iw=img.naturalWidth*scale,ih=img.naturalHeight*scale;"
       "img.style.width=iw+'px';img.style.height=ih+'px';"
       "img.style.left=Math.min(0,Math.max(w-iw,w/2-Number(box.dataset.x)*iw))+'px';"
       "img.style.top=Math.min(0,Math.max(h-ih,h/2-Number(box.dataset.y)*ih))+'px';};"
       "draw();new ResizeObserver(draw).observe(box);}</script><h1>" (escape-html (-> report :post :title)) "</h1>"
       "<p>Dry run · " (escape-html model) (if cached? " · cached" " · newly generated")
       " · <a href=\"" (escape-html (:report-overview-uri report)) "\">BEU report</a>"
       " · <a href=\"report.pdf\">Source PDF</a> · <a href=\"comparison.json\">JSON drafts</a></p>"
       (when (seq validation-errors)
         (str "<section class=\"error\"><h2>AI proposal rejected — not a valid draft</h2><p>"
              (escape-html (str/join "; " validation-errors))
              "</p><p>The comparison below is for diagnosis only. Saved attempt JSON files contain the model outputs.</p></section>"))
       (when source-sections
         (str "<section><h2>Original report sections</h2>"
              "<p>Repaired source extraction, with page headers, footers and figure captions removed for composition. "
              "Known glyphs are repaired; unfamiliar fonts can still contain extraction errors.</p>"
              (str/join (map (fn [{:keys [title text pages]}]
                               (str "<h3>" (escape-html title) "</h3>"
                                    (if (seq pages)
                                      (str "<p>"
                                           (str/join " · " (map #(str "<a href=\"report.pdf#page=" % "\">PDF page " % "</a>") pages))
                                           "</p><pre>" (escape-html text) "</pre>")
                                      "<p>Section not found automatically. Please check the source PDF.</p>")))
                             source-sections)) "</section>"))
       (when raw-source-sections
         (str "<details><summary>Original extraction before font repair</summary>"
              (str/join (map #(str "<h3>" (escape-html (:title %)) "</h3><pre>" (escape-html (:text %)) "</pre>")
                             raw-source-sections)) "</details>"))
       (when (:repair report)
         (str "<p>Font repair: " (-> report :repair :mappings-added) " missing mappings restored; " (-> report :repair :mappings-corrected) " existing symbol mappings corrected. "
              "Full repaired page text is used in image alt text. "
              "<a href=\"" (escape-html (str (last (butlast (str/split (:repaired-pdf-path report) #"/"))) "/repaired.pdf"))
              "\">Repaired PDF copy</a></p>"))
       (when image-candidates
         (str "<section><h2>Image selection roles</h2><pre>" (escape-html (json image-candidates))
              "</pre><p>Page 1 and the detected summary are fixed; missing photo/map slots use following pages. Final pages are unique and sorted.</p></section>"))
       (when image-verifications
         (str "<details><summary>Single-page visual checks</summary><pre>" (escape-html (json image-verifications))
              "</pre><h3>Original whole-report proposal</h3><pre>" (escape-html (json proposed-image-candidates)) "</pre></details>"))
       (when (:missing-source? text-policy)
         "<p class=\"error\">No event-description section was detected. Mastodon shows only the headline for image review; this is not a publishable text draft.</p>")
       (when text-policy
         (str "<p>Text: " (if (= "shortened" (name (:mode text-policy))) "AI shortened because the original exceeded the platform limit"
                              "copied programmatically from the repaired source")
              ". Folgen: " (if (:included-consequences? text-policy) "included" "omitted") ".</p>"))
       "<main><section><h2>Original programmatic baseline</h2><p>Original template with the full URL, before link shortening.</p>"
       (str/join (map (fn [[platform post]] (post-html platform post)) (:posts baseline)))
       (str/join (map page-html (:pages baseline))) "</section><section><h2>Shared bot / preview · source text + AI images</h2>"
       (str/join (map (fn [[platform post]] (post-html platform post)) (:posts ai)))
       (str/join (map page-html (:pages ai))) "<h3>Supporting quotations</h3>"
       (str/join (map (fn [{:keys [claim page quote]}]
                        (str "<p>" (escape-html claim) " — PDF page " page
                             "</p><blockquote>" (escape-html quote) "</blockquote>"))
                      (:evidence ai))) "</section></main></html>"))

(defn original-posts [report limits]
  ;; Show exactly the old template, with full URL and all tags, even if it is too long.
  (let [title (-> report :post :title)
        tags (-> report :post :tags)
        mastodon (draft/compose :mastodon title report tags limits)
        bsky-text (:text mastodon)]
    {:mastodon mastodon
     :bluesky {:text bsky-text :uri (:report-overview-uri report)
               :length (draft/grapheme-count bsky-text) :max-length 300
               :bytes (draft/byte-count bsky-text)
               :fits? (and (<= (draft/grapheme-count bsky-text) 300)
                           (<= (draft/byte-count bsky-text) 3000))}}))

(defn render-comparison+ [baseline proposed]
  ;; Render shared pages once; concurrent Poppler writers to the same path can corrupt images.
  (let [pages (->> (concat (:interesting-pages baseline) (:interesting-pages proposed))
                   (reduce (fn [by-page page] (assoc by-page (:page page) page)) {}) vals vec)]
    (-> (pdf/add-screenshots+ (assoc baseline :interesting-pages pages))
        (.then (fn [rendered]
                 (let [by-page (into {} (map (juxt :page identity) (:interesting-pages rendered)))
                       attach (fn [report]
                                (update report :interesting-pages
                                        #(mapv (fn [page]
                                                 (merge (select-keys (get by-page (:page page))
                                                                     [:image-path :content-type :image-size])
                                                        page)) %)))]
                   [(attach baseline) (attach proposed)]))))))

(def selected-pages preview/selected-pages)

(defn save-failure-review+ [prepared limits opts directory error]
  (let [generated (:generated (ex-data error))
        result (:result generated)
        errors (or (:validation-errors (ex-data error)) [(or (ex-message error) (.-message error))])
        valid-pages (->> (:pages result)
                         (filter #(and (integer? (:page %)) (<= 1 (:page %) (:page-count prepared))))
                         (reduce (fn [pages page] (assoc pages (:page page) page)) {}) vals
                         (selected-pages prepared))
        proposed (assoc prepared :interesting-pages valid-pages)
        baseline (pdf/select-interesting-pages+ prepared)]
    (-> (render-comparison+ baseline proposed)
        (.then (fn [[baseline proposed]]
                 (let [comparison {:source-sections (sections/extract-clean prepared)
                                   :raw-source-sections (sections/extract (assoc prepared :pages (:raw-pages prepared)))
                                   :report (dissoc prepared :pages :raw-pages) :model (:model opts)
                                   :accepted? false :validation-errors errors :limits limits
                                   :usage (:usage generated) :attempt-usages (:attempt-usages generated)
                                   :baseline {:posts (original-posts prepared limits)
                                              :pages (:interesting-pages baseline)}
                                   :ai {:posts (if (every? string? ((juxt :bluesky :mastodon) result))
                                                 (draft/draft-posts result prepared (-> prepared :post :tags) limits) {})
                                        :pages (:interesting-pages proposed) :evidence (:evidence result)}}]
                   (-> (write-json+ (str directory "/comparison.json") comparison)
                       (.then #(fs/writeFile (str directory "/index.html") (comparison-html comparison)))))))
        (.then (fn []
                 (throw (ex-info (str (or (ex-message error) (.-message error))
                                      " — review " directory "/index.html") (ex-data error))))))))

(defn preview-one+ [report limits opts]
  (let [opts (assoc opts :image-policy :roles :image-review? true)
        id (subs (:report-id report) 4 14)
        directory (str (:out opts) "/" id)
        prepared-state (atom nil)]
    (js/console.log (str "Preparing " (:event-location report) " (" id ")"))
    (-> (source-preview/prepare+ report directory)
        (.then (fn [prepared]
                 (reset! prepared-state prepared)
                 (-> (source-preview/cached-generation+ prepared limits opts directory)
                     (.then (fn [{:keys [result posts usage attempt-usages cached? response-id text-policy image-candidates proposed-image-candidates image-verifications]}]
                              (let [baseline (pdf/select-interesting-pages+ prepared)
                                    proposed (assoc prepared :interesting-pages
                                                    (selected-pages prepared (:pages result)))]
                                (-> (render-comparison+ baseline proposed)
                                    (.then (fn [[baseline proposed]]
                                             (let [comparison {:source-sections (sections/extract-clean prepared)
                                                               :raw-source-sections (sections/extract (assoc prepared :pages (:raw-pages prepared)))
                                                               :report (dissoc prepared :pages :raw-pages)
                                                               :model (:model opts) :cached? cached? :accepted? true
                                                               :limits limits :usage usage :attempt-usages attempt-usages
                                                               :response-id response-id :text-policy text-policy :text-accepted? (not (:missing-source? text-policy)) :image-candidates image-candidates
                                                               :proposed-image-candidates proposed-image-candidates :image-verifications image-verifications
                                                               :baseline {:posts (original-posts prepared limits)
                                                                          :pages (:interesting-pages baseline)}
                                                               :ai {:posts posts :pages (:interesting-pages proposed)
                                                                    :evidence (:evidence result)}}]
                                               (-> (write-json+ (str directory "/comparison.json") comparison)
                                                   (.then #(fs/writeFile (str directory "/index.html")
                                                                         (comparison-html comparison)))
                                                   (.then (fn []
                                                            (js/console.log (str "Saved " directory "/index.html"
                                                                                 (when cached? " (cached)")))
                                                            {:id id :directory directory :ok? true})))))))))))))
        (.catch (fn [error]
                  (if-let [prepared @prepared-state]
                    (save-failure-review+ prepared limits opts directory error)
                    (throw error)))))))

(defn run+ [opts]
  (let [base-uri (or (.. js/process -env -MASTO_BASE_URI) "https://zug.network")]
    (-> (js/Promise.all [(reports+ opts) (draft/mastodon-limits+ base-uri)])
        (.then (fn [[reports limits]]
                 (when (empty? reports)
                   (throw (ex-info "No reports selected" {})))
                 (js/console.log (str "Dry run: " (count reports) " report(s), " (:model opts)
                                      "; Mastodon limit " (:max_characters limits)
                                      ", Bluesky limit 300. No publishing."))
                 ;; Sequential to bound spend and Poppler processes.
                 (reduce (fn [previous report]
                           (.then previous
                                  (fn [results]
                                    (-> (preview-one+ report limits opts)
                                        (.then #(conj results %))
                                        (.catch (fn [error]
                                                  (let [message (or (ex-message error) (.-message error))]
                                                    (js/console.error (str (:report-id report) ": " message))
                                                    (conj results {:id (:report-id report) :ok? false :error message}))))))))
                         (js/Promise.resolve []) reports)))
        (.then (fn [results]
                 (-> (fs/mkdir (:out opts) #js {:recursive true})
                     (.then #(write-json+ (str (:out opts) "/summary.json") results))
                     (.then (fn []
                              (when (some #(not (:ok? %)) results)
                                (set! (.-exitCode js/process) 1))
                              results))))))))

(defn main []
  (try
    (let [opts (parse-args (drop 2 (js->clj (.-argv js/process))))]
      (if (:help? opts)
        (js/console.log help-text)
        (-> (run+ opts)
            (.catch (fn [error]
                      (js/console.error (or (ex-message error) (.-message error)))
                      (set! (.-exitCode js/process) 1))))))
    (catch :default error
      (js/console.error (or (ex-message error) (.-message error)))
      (set! (.-exitCode js/process) 1))))
