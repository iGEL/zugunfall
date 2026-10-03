(ns bot.dry-run
  (:require ["fs/promises" :as fs]
            [bot.ai :as ai]
            [bot.beu :as beu]
            [bot.date :as date]
            [bot.pdf :as pdf]
            [bot.post-draft :as draft]
            [bot.report-preview :as preview]
            [clojure.string :as str]))

(def help-text
  (str "AI preview dry run — never publishes or accesses social-network accounts.\n\n"
       "npm run build:dry-run\n"
       "OPENAI_API_KEY=... npm run dry-run -- [options]\n\n"
       "  --count N        Latest N reports, default 1 (maximum 20)\n"
       "  --manifest FILE  JSON array of report metadata, optionally with pdf-path\n"
       "  --out DIR        Output directory, default reports/ai-preview\n"
       "  --model NAME     OpenAI vision model, default gpt-4.1-mini\n"
       "  --refresh        Regenerate even if a matching cached draft exists\n"
       "  --help           Show this help\n\n"
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
        (contains? #{"--count" "--out" "--model" "--manifest"} arg)
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
      opts)))

(defn validate-report [report]
  (when-not (and (re-matches #"\[id:[a-zA-Z0-9_-]{10}\]" (or (:report-id report) ""))
                 (every? #(and (string? %) (not (str/blank? %)))
                         ((juxt :report-type :event-type :event-date :event-location :report-date) report))
                 (every? #(and (string? %) (re-matches #"https?://\S+" %))
                         ((juxt :report-overview-uri :report-pdf-uri) report)))
    (throw (ex-info "Invalid report metadata; see the README manifest example" {})))
  report)

(defn reports+ [{:keys [manifest count]}]
  (if manifest
    (-> (fs/readFile manifest "utf8")
        (.then (fn [text]
                 (let [reports (js->clj (.parse js/JSON text) :keywordize-keys true)]
                   (when-not (vector? reports)
                     (throw (ex-info "Manifest must be a JSON array" {})))
                   (mapv validate-report (take count reports))))))
    (-> (js/Promise.all [(beu/fetch-reports+ "Untersuchungsbericht")
                         (beu/fetch-reports+ "Zwischenbericht")])
        (.then #(->> % (mapcat identity)
                     (sort-by (fn [report] (date/german->iso (:report-date report))))
                     reverse (take count)))
        (.then beu/fetch-reports-details+)
        (.then #(mapv validate-report %)))))

(def json preview/json)
(def write-json+ preview/write-json+)
(def read-json-if-present+ preview/read-json-if-present+)
(def prepare-pdf+ preview/prepare-pdf+)
(def cache-key preview/cache-key)
(def generate-fitting+ preview/generate-fitting+)
(def cached-generation+ preview/cached-generation+)

(defn escape-html [value]
  (-> (str value) (str/replace "&" "&amp;") (str/replace "<" "&lt;")
      (str/replace ">" "&gt;") (str/replace "\"" "&quot;")
      (str/replace "'" "&#39;")))

(defn page-html [{:keys [page image-path text alt image-description reason]}]
  (str "<figure><a href=\"" (escape-html (last (str/split image-path #"/")))
       "\"><img loading=\"lazy\" src=\"" (escape-html (last (str/split image-path #"/")))
       "\" alt=\"" (escape-html (or alt text)) "\"></a><figcaption>PDF page " page
       (when reason (str " — " (escape-html reason))) "</figcaption>"
       (when-not (str/blank? image-description)
         (str "<p>Bildbeschreibung: " (escape-html image-description) "</p>"))
       "<details><summary>Full page text</summary><pre>" (escape-html text) "</pre></details>"
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

(defn comparison-html [{:keys [report baseline ai model cached? validation-errors]}]
  (str "<!doctype html><html lang=\"en\"><meta charset=\"utf-8\">"
       "<meta name=\"viewport\" content=\"width=device-width, initial-scale=1\">"
       "<title>BEU preview comparison</title><style>"
       "body{font:16px system-ui;margin:2rem;background:#f6f7f8;color:#20252b}"
       "main{display:grid;grid-template-columns:repeat(2,minmax(0,1fr));gap:2rem}"
       "section{background:white;padding:1.5rem;border-radius:12px}"
       "pre{white-space:pre-wrap;overflow-wrap:anywhere;font:inherit;line-height:1.5}"
       "figure{margin:1rem 0}img{width:100%;max-height:620px;object-fit:contain;background:#eee}"
       "figcaption{margin:.5rem 0}.ok{color:#176636}.error{color:#b02020}"
       "@media(max-width:800px){main{grid-template-columns:1fr}body{margin:1rem}}"
       "</style><h1>" (escape-html (-> report :post :title)) "</h1>"
       "<p>Dry run · " (escape-html model) (if cached? " · cached" " · newly generated")
       " · <a href=\"" (escape-html (:report-overview-uri report)) "\">BEU report</a>"
       " · <a href=\"report.pdf\">Source PDF</a> · <a href=\"comparison.json\">JSON drafts</a></p>"
       (when (seq validation-errors)
         (str "<section class=\"error\"><h2>AI proposal rejected — not a valid draft</h2><p>"
              (escape-html (str/join "; " validation-errors))
              "</p><p>The comparison below is for diagnosis only. Saved attempt JSON files contain the model outputs.</p></section>"))
       "<main><section><h2>Current bot</h2><p>Original template with the full URL, before link shortening.</p>"
       (str/join (map (fn [[platform post]] (post-html platform post)) (:posts baseline)))
       (str/join (map page-html (:pages baseline))) "</section><section><h2>AI proposal</h2>"
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
                 (let [comparison {:report (dissoc prepared :pages) :model (:model opts)
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
  (let [id (subs (:report-id report) 4 14)
        directory (str (:out opts) "/" id)
        prepared-state (atom nil)]
    (js/console.log (str "Preparing " (:event-location report) " (" id ")"))
    (-> (prepare-pdf+ report directory)
        (.then (fn [prepared]
                 (reset! prepared-state prepared)
                 (-> (cached-generation+ prepared limits opts directory)
                     (.then (fn [{:keys [result posts usage attempt-usages cached? response-id]}]
                              (let [baseline (pdf/select-interesting-pages+ prepared)
                                    proposed (assoc prepared :interesting-pages
                                                    (selected-pages prepared (:pages result)))]
                                (-> (render-comparison+ baseline proposed)
                                    (.then (fn [[baseline proposed]]
                                             (let [comparison {:report (dissoc prepared :pages)
                                                               :model (:model opts) :cached? cached? :accepted? true
                                                               :limits limits :usage usage :attempt-usages attempt-usages
                                                               :response-id response-id
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
