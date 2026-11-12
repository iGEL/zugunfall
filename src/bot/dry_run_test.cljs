(ns bot.dry-run-test
  (:require ["fs/promises" :as fs]
            [bot.ai :as ai]
            [bot.image-selection :as images]
            [bot.image-focus :as focus]
            [bot.beu :as beu]
            [bot.bsky :as bsky]
            [bot.dry-run :as dry]
            [bot.main :as bot]
            [bot.mastodon :as mastodon]
            [bot.post-draft :as draft]
            [bot.source-sections :as sections]
            [bot.source-preview :as source-preview]
            [cljs.test :as test :refer-macros [async deftest is testing]]
            [clojure.string :as str]))

(def limits {:max_characters 5000 :characters_reserved_per_url 23 :max_media_attachments 4})
(def report
  {:report-id "[id:0123456789]" :report-type "Untersuchungsbericht"
   :event-type "Entgleisung" :event-location "Teststadt" :event-date "01.01.2026"
   :report-date "02.01.2026" :report-overview-uri "https://example.org/very/long/report/link"
   :report-pdf-uri "https://example.org/report.pdf"
   :page-count 1 :pages [{:page 1 :text "Ein Zug entgleiste in Teststadt. Niemand wurde verletzt."}]})
(def result
  {:bluesky (ai/headline report)
   :mastodon (str (ai/headline report) "\n\nEin Zug entgleiste in Teststadt. Niemand wurde verletzt.")
   :pages [{:page 1 :reason "Anschauliche Skizze" :image-description ""}]
   :evidence [{:page 1 :claim "Ein Zug entgleiste in Teststadt."
               :quote "Ein Zug entgleiste in Teststadt."}]})

(deftest report-url-and-original-sections
  (let [url "https://www.eisenbahn-unfalluntersuchung.de/SharedDocs/Downloads/BEU/Zwischenberichte/2025/example.html"
        html "<main id='main'><p class='info'>Thema: Zugentgleisung, Ereignis vom: 19.09.2025, Publikation vom: 09.09.2026</p><h1>Emmerich</h1><a class='downloadLink' href='/report.pdf?__blob=publicationFile&amp;amp;v=2'>PDF</a></main>"
        parsed (beu/extract-report-page url {:body html})]
    (is (= "Zwischenbericht" (:report-type parsed)))
    (is (= "Zugentgleisung" (:event-type parsed)))
    (is (= "Emmerich" (:event-location parsed)))
    (is (= "19.09.2025" (:event-date parsed)))
    (is (= "09.09.2026" (:report-date parsed)))
    (is (= "https://www.eisenbahn-unfalluntersuchung.de/report.pdf?__blob=publicationFile&v=2"
           (:report-pdf-uri parsed)))
    (is (= url (beu/overview-url (str/replace url #"\.html$" ".pdf?__blob=publicationFile&v=2"))))
    (is (thrown? js/Error (beu/overview-url "https://example.org/report.pdf")))
    (is (thrown? js/Error (beu/extract-report-page url {:body "<html></html>"}))))
  (let [[summary consequences]
        (sections/extract {:pages [{:page 1 :text "1.1 Kurzbeschreibung des Ereignisses .... 2\n1.2 Folgen .... 3"}
                                   {:page 2 :text "1.1 Kurzbeschreibung des Ereignisses\nEin Zug mit\n13 Wagen.\nWeiter"}
                                   {:page 3 :text "ging die Fahrt.\n1.2 Folgen\nNiemand wurde verletzt.\n1.3 Ursachen\nKeine Angabe."}]})]
    (is (= [2 3] (:pages summary)))
    (is (= "Ein Zug mit\n13 Wagen.\nWeiter\nging die Fahrt." (:text summary)))
    (is (= "Niemand wurde verletzt." (:text consequences)))
    (is (= [3] (:pages consequences))))
  (is (= [] (:pages (first (sections/extract report)))))
  (is (= "https://example.org" (:url (dry/parse-args ["--url" "https://example.org"]))))
  (is (thrown? js/Error (dry/parse-args ["--url" "x" "--manifest" "x"]))))

(deftest unicode-counting-and-facets
  (is (= 1 (draft/grapheme-count "👨‍👩‍👧‍👦")))
  (is (= 1 (draft/grapheme-count "é")))
  (is (> (draft/byte-count "🚂") (draft/grapheme-count "🚂")))
  (let [post (draft/compose :bluesky "🚂 Über den Unfall" report ["BahnBubble" "BEU"] limits)
        bytes (js/Buffer.from (:text post) "utf8")]
    (is (:fits? post))
    (doseq [[facet expected] (map vector (:facets post) [draft/link-label "#BahnBubble" "#BEU"])]
      (is (= expected (.toString (.subarray bytes (-> facet :index :byteStart)
                                            (-> facet :index :byteEnd)) "utf8"))))
    (is (= (:report-overview-uri report) (-> post :facets first :features first :uri)))))

(deftest complete-message-limits
  (let [budget (draft/body-budget :bluesky report limits)
        body (str/join (repeat budget "ü"))
        post (draft/fit-post :bluesky body report ["BEU" "BahnBubble"] limits)]
    (is (= 300 (:length post)))
    (is (:fits? post))
    (is (= [] (:tags post)))
    (is (= ["BEU" "BahnBubble"] (:dropped-tags post)))
    (is (not (:fits? (draft/fit-post :bluesky (str body "!") report [] limits))))
    (is (str/includes? (:text post) (:report-id report))))
  (let [budget (draft/body-budget :mastodon report limits)
        post (draft/fit-post :mastodon (str/join (repeat budget "a")) report ["BEU"] limits)]
    (is (:fits? post))
    (is (= 5000 (:length post)))
    (is (= [] (:tags post))))
  (testing "Mastodon reserves configured URL length, irrespective of the actual URL length"
    (is (= (:length (draft/compose :mastodon "Text" report [] limits))
           (:length (draft/compose :mastodon "Text" (assoc report :report-overview-uri
                                                           (str "https://example.org/" (str/join (repeat 400 "a"))))
                                   [] limits)))))
  (testing "Bluesky also enforces the independent UTF-8 byte limit"
    (let [body (str/join (repeat 250 "👨‍👩‍👧‍👦"))
          post (draft/fit-post :bluesky body report [] limits)]
      (is (<= (:length post) 300))
      (is (> (:bytes post) 3000))
      (is (not (:fits? post))))))

(deftest full-page-text-and-selected-order
  (let [text (str "Vollständiger Seitentext: " (str/join (repeat 1000 "ä")))
        pdf-report (assoc report :pages [{:page 1 :text text}
                                         {:page 2 :text "Zweite Seite"}
                                         {:page 3 :text "Dritte Seite"}])
        ordered (dry/selected-pages pdf-report [{:page 1 :reason "Ereignis"}
                                                {:page 3 :reason "Gleisplan"}])
        reordered (dry/selected-pages pdf-report [{:page 3 :reason "Gleisplan erklärt die folgende Ereignisbeschreibung"}
                                                  {:page 1 :reason "Ereignis" :alt "Model description" :text "Model transcription"}])]
    (is (= [1 3] (mapv :page ordered)))
    (is (= [3 1] (mapv :page reordered)))
    (is (= text (-> ordered first :alt)))
    (is (= text (-> reordered second :alt)))
    (is (= text (-> reordered second :text)))
    (is (> (draft/grapheme-count (-> ordered first :alt)) 800))
    (let [html (dry/page-html (assoc (first ordered) :image-path "report.pdf-1.png"))]
      (is (str/includes? html (str "alt=\"" text "\"")))
      (is (str/includes? html "Full page text"))
      (is (str/includes? html (str "<pre>" text "</pre>"))))))

(deftest visual-description-plus-full-text
  (let [text (str "Originaltext <&>\n" (str/join (repeat 1200 "ü")))
        description "Gleisplan: Gleis 1 verläuft links neben Gleis 2."
        pdf-report (assoc report :pages [{:page 1 :text text}])
        illustrated (first (dry/selected-pages pdf-report [{:page 1 :reason "Gleisplan"
                                                            :image-description (str "  " description "  ")
                                                            :text "AI transcription" :alt "AI full alt"}]))
        expected (str "Bildbeschreibung:\n" description "\n\nText der Seite:\n" text)]
    (is (= description (:image-description illustrated)))
    (is (= text (:text illustrated)))
    (is (= expected (:alt illustrated)))
    (is (= text (:alt (first (dry/selected-pages pdf-report [{:page 1 :reason "Text" :image-description "  "}])))))
    (let [html (dry/page-html (assoc illustrated :image-path "report.pdf-1.png"))]
      (is (str/includes? html (str "alt=\"" (dry/escape-html expected) "\"")))
      (is (str/includes? html (str "<p>Bildbeschreibung: " description "</p>")))
      (is (str/includes? html (str "<pre>" (dry/escape-html text) "</pre>"))))))

(deftest response-errors
  (is (= result (ai/parse-response {:status "completed"
                                    :output [{:content [{:type "output_text" :text (dry/json result)}]}]})))
  (is (thrown? js/Error (ai/parse-response {:status "incomplete" :output []})))
  (is (thrown? js/Error (ai/parse-response {:status "completed"
                                            :output [{:content [{:type "refusal"}]}]})))
  (is (thrown? js/Error (ai/parse-response {:status "completed" :output []}))))

(deftest cache-and-cli
  (is (= 2 (:count (dry/parse-args ["--count" "2"]))))
  (is (:refresh? (dry/parse-args ["--refresh"])))
  (is (thrown? js/Error (dry/parse-args ["--count" "1.5"])))
  (is (thrown? js/Error (dry/parse-args ["--count" "0"])))
  (is (thrown? js/Error (dry/parse-args ["--model"])))
  (is (thrown? js/Error (dry/validate-report (assoc report :report-id "../../bad"))))
  (let [opts {:model "model-a"}
        key (source-preview/cache-key (assoc report :pdf-content-hash "pdf") limits opts)]
    (is (not= key (source-preview/cache-key (assoc report :pdf-content-hash "pdf") limits {:model "model-b"})))
    (is (not= key (source-preview/cache-key (assoc report :pdf-content-hash "pdf") limits (assoc opts :image-policy :roles))))
    (is (not= key (source-preview/cache-key (assoc report :pdf-content-hash "changed-pdf") limits opts)))
    (is (not= key (source-preview/cache-key (assoc report :pdf-content-hash "pdf") (assoc limits :max_characters 500) opts)))
    (is (not= key (source-preview/cache-key (assoc report :pdf-content-hash "pdf" :pages [{:page 1 :text "Repaired source"}]) limits opts))))
  (is (= "&lt;script&gt;&amp;&quot;&#39;" (dry/escape-html "<script>&\"'"))))

(defn sample-pdf
  ([] (sample-pdf ["Ein Zug entgleiste in Teststadt." "Niemand wurde verletzt."]))
  ([lines]
  ;; A tiny real PDF exercises Poppler, image rendering, quotations and review artifacts.
   (let [stream (str "BT /F1 12 Tf 40 260 Td " (str/join " 0 -20 Td " (map #(str "(" % ") Tj") lines)) " ET")
         objects ["<< /Type /Catalog /Pages 2 0 R >>"
                  "<< /Type /Pages /Kids [3 0 R] /Count 1 >>"
                  "<< /Type /Page /Parent 2 0 R /MediaBox [0 0 300 300] /Resources << /Font << /F1 4 0 R >> >> /Contents 5 0 R >>"
                  "<< /Type /Font /Subtype /Type1 /BaseFont /Helvetica >>"
                  (str "<< /Length " (count stream) " >>\nstream\n" stream "\nendstream")]
         [body offsets] (reduce (fn [[body offsets] [idx object]]
                                  [(str body (inc idx) " 0 obj\n" object "\nendobj\n")
                                   (conj offsets (count body))])
                                ["%PDF-1.4\n" []] (map-indexed vector objects))]
     (str body "xref\n0 6\n0000000000 65535 f \n"
          (str/join (map #(str (.padStart (str %) 10 "0") " 00000 n \n") offsets))
          "trailer\n<< /Size 6 /Root 1 0 R >>\nstartxref\n" (count body) "\n%%EOF\n"))))

(def source-report
  (assoc report :pages [{:page 1 :text "1.1 Kurzbeschreibung des Ereignisses\nEin Zug entgleiste in Teststadt.\n1.2 Folgen\nNiemand wurde verletzt.\n1.3 Ursachen\nAndere Details."}]))

(deftest programmatic-source-composition
  (let [plan (source-preview/text-plan source-report limits)]
    (is (= (ai/headline report) (:bluesky plan)))
    (is (= (str (ai/headline report) "\n\nEin Zug entgleiste in Teststadt.\n\nNiemand wurde verletzt.") (:mastodon plan)))
    (is (:included-consequences? plan))
    (is (not (:needs-shortening? plan))))
  (let [long-source (assoc source-report :pages [{:page 1 :text (str "1.1 Kurzbeschreibung des Ereignisses\n" (str/join (repeat 70 "Ein Satz zum Ereignis. "))
                                                                     "\n1.2 Folgen\nNiemand wurde verletzt.")}])
        plan (source-preview/text-plan long-source limits)]
    (is (not (:included-consequences? plan)))
    (is (not (:needs-shortening? plan))))
  (let [tagged (assoc source-report :post {:tags [(str/join (repeat 100 "BEU"))]})
        plan (source-preview/text-plan tagged limits)
        exact-limit (:length (draft/compose :mastodon (:mastodon plan) tagged [] limits))]
    (is (not (:needs-shortening? (source-preview/text-plan tagged (assoc limits :max_characters exact-limit))))))
  (is (thrown? js/Error (source-preview/text-plan report limits)))
  (is (= "offen und m² ①" (sections/prose "oﬀen\nund m² ①"))))

(deftest image-selection-validation
  (is (empty? (source-preview/selection-errors {:pages (:pages result)} report limits)))
  (doseq [pages [[] [(assoc (first (:pages result)) :page 99)]
                 (conj (:pages result) (first (:pages result)))
                 [(assoc (first (:pages result)) :reason "")]
                 [(dissoc (first (:pages result)) :image-description)]]]
    (is (seq (source-preview/selection-errors {:pages pages} report limits)))))

(deftest programmatic-headlines
  (doseq [report-type ["Untersuchungsbericht" "Zwischenbericht"]]
    (let [metadata (assoc source-report :report-type report-type :event-date "19.09.2025" :event-location "Emmerich")
          plan (source-preview/text-plan metadata limits)
          expected (str report-type " über Entgleisung am 19.09.2025 in Emmerich")]
      (is (= expected (:bluesky plan)))
      (is (= expected (first (str/split-lines (:mastodon plan))))))))

(deftest source-formatting-and-alt-text
  (is (= "Containertragwagen sowie Leit- und Sicherungstechnik"
         (sections/prose "Container-\ntragwagen sowie Leit-\nund Sicherungstechnik")))
  (let [page {:page 1 :text "Rotterdam: Schotterflug. Vollständiger Seitentext." :raw-text "Ro erdam: Scho erflug. Vollständiger Seitentext."}
        selected (dry/selected-pages (assoc report :pages [page])
                                     [{:page 1 :reason "Foto" :image-description "Ein Wagen."}])]
    (is (= (:text page) (-> selected first :text)))
    (is (str/includes? (-> selected first :alt) (:text page)))
    (is (not (str/includes? (-> selected first :alt) "Ro erdam")))
    (is (= (:raw-text page) (-> selected first :raw-text)))))

(deftest bullet-list-reconstruction
  (let [raw "Beteiligte:\n•\n\ndas erste Unternehmen,\n•\n\ndas zweite Unternehmen\n(Abteilung),\n• der Hersteller\n\nDanach folgt ein Absatz."
        expected "Beteiligte:\n• das erste Unternehmen,\n• das zweite Unternehmen (Abteilung),\n• der Hersteller\n\nDanach folgt ein Absatz."]
    (is (= expected (sections/prose raw)))
    (is (= expected (sections/prose expected)))
    (is (str/includes? (sections/page-text raw) "• das zweite Unternehmen\n(Abteilung),"))
    (is (= (sections/page-text raw) (sections/page-text (sections/page-text raw))))
    (let [prepared (assoc report :pages [{:page 1 :text (sections/page-text raw)}])
          selected (dry/selected-pages prepared [{:page 1 :reason "Liste" :image-description ""}])]
      (is (= (sections/page-text raw) (-> selected first :alt)))))
  (is (= "1.2 Folgen\n•\n2.3 Untersuchungsfortgang"
         (sections/page-text "1.2 Folgen\n•\n2.3 Untersuchungsfortgang")))
  (is (= "Formel: a • b und m²." (sections/prose "Formel: a • b und m².")))
  (is (= "\uf0b7 fremdes Zeichen" (sections/page-text "\uf0b7 fremdes Zeichen")))
  (is (= "• erster Punkt\n▪ zweiter Punkt"
         (sections/prose "• erster Punkt\n▪\n\nzweiter Punkt"))))

(deftest role-based-image-resolution
  (let [report (assoc source-report :page-count 10 :pages [{:page 5 :text (:text (first (:pages source-report)))}])
        page (fn [n] {:page n :reason "Candidate" :image-description "Visible content"})
        candidates {:summary (page 2) :photo (page 9) :map (page 7) :descriptions []}
        selected #(images/resolve-pages report limits %)]
    (is (= 5 (images/summary-page report)))
    (is (= [1 5 7 9] (mapv :page (selected candidates))))
    (is (= [1 5 6 9] (mapv :page (selected (assoc candidates :map nil)))))
    (is (= [1 5 6 7] (mapv :page (selected (assoc candidates :photo nil :map nil)))))
    (is (= [1 5 6 7] (mapv :page (selected (assoc candidates :photo (page 5) :map (page 5))))))
    (is (= [1 5] (mapv :page (images/resolve-pages report (assoc limits :max_media_attachments 2) candidates))))
    (is (= [1 2 7 9] (mapv :page (images/resolve-pages (assoc report :pages []) limits candidates))))
    (is (seq (images/errors (assoc candidates :photo (page 11)) report))))
  (let [page {:page 1 :reason "Only page" :image-description ""}
        candidates {:summary page :photo nil :map nil :descriptions [{:page 1 :image-description ""}]}]
    (is (= [1] (mapv :page (images/resolve-pages source-report limits candidates))))
    (is (empty? (images/errors candidates source-report)))
    (is (seq (images/errors (assoc candidates :summary nil) (assoc source-report :pages [])))))
  (is (:missing-source? (source-preview/text-plan report limits true))))

(deftest photo-selection-needs-image-content
  (let [inventory "page num type width height\n 16 0 image 945 659\n 19 1 image 946 553\n 19 2 smask 946 553\n 20 3 mask 200 100\n 19 4 image 946 399"
        report (assoc source-report :page-count 64 :pages [{:page 9 :text (-> source-report :pages first :text)}]
                      :raster-image-pages (images/raster-pages inventory))
        candidate {:summary {:page 9 :reason "Summary" :image-description ""}
                   :photo {:page 18 :reason "Luftbild der Unfallstelle" :image-description "Fotos der Unfallstelle."}
                   :map {:page 16 :reason "Lageplan" :image-description "Karte der Unfallstelle."}
                   :descriptions (mapv #(hash-map :page % :image-description "") [1 9 10 11 12])}]
    (is (= #{16 19} (:raster-image-pages report)))
    (is (some #(str/includes? % "page 18 contains no embedded images") (images/errors candidate report)))
    (is (empty? (images/errors (assoc-in candidate [:photo :page] 19) report)))
    (is (= [1 9 16 19] (mapv :page (images/resolve-pages report limits (assoc-in candidate [:photo :page] 19)))))))

(deftest conditional-shortening-only
  (async done
         (let [original ai/request+
               old-key (.. js/process -env -OPENAI_API_KEY)
               calls (atom [])
               long-report (assoc source-report :pages [{:page 1 :text (str "1.1 Kurzbeschreibung des Ereignisses\n"
                                                                            (str/join (repeat 100 "Ein Zug entgleiste in Teststadt. "))
                                                                            "\n1.2 Folgen\nNiemand wurde verletzt.")}])]
           (set! (.. js/process -env -OPENAI_API_KEY) "offline-key")
           (set! ai/request+
                 (fn [body _]
                   (swap! calls conj body)
                   (js/Promise.resolve
                    {:status "completed" :usage {:input_tokens 1 :output_tokens 1}
                     :output [{:content [{:type "output_text"
                                          :text (dry/json (if (= "shorten_source" (-> body :text :format :name))
                                                            {:text (if (= 1 (count @calls)) (str/join (repeat 500 "zu lang ")) "Ein Zug entgleiste in Teststadt.")}
                                                            {:pages (:pages result)}))}]}]})))
           (-> (fs/mkdtemp "/tmp/zugunfall-shortening-")
               (.then (fn [folder]
                        (let [path (str folder "/source.pdf")]
                          (-> (fs/writeFile path (sample-pdf))
                              (.then #(source-preview/generate+ (assoc long-report :pdf-path path)
                                                                (assoc limits :max_characters 250) {:model "stub"} folder))))))
               (.then (fn [generated]
                        (is (= ["shorten_source" "shorten_source" "select_report_images"]
                               (mapv #(-> % :text :format :name) @calls)))
                        (is (= "input_text" (-> @calls first :input first :content first :type)))
                        (is (= (ai/headline report) (-> generated :result :bluesky)))
                        (is (= :shortened (-> generated :text-policy :mode)))
                        (is (every? :fits? (vals (:posts generated))))))
               (.catch #(is false (str %)))
               (.finally (fn [] (set! ai/request+ original)
                           (if old-key (set! (.. js/process -env -OPENAI_API_KEY) old-key)
                               (js-delete (.-env js/process) "OPENAI_API_KEY")) (done)))))))

(def text-page-finding
  {:has-incident-photo false :photo-overview-score 0 :has-location-map false
   :photo-focus nil :map-focus nil :image-description "" :reason "Text only."})

(deftest isolated-photo-verification-allows-mixed-pages
  (async done
         (let [report (assoc source-report :page-count 40 :pages [{:page 9 :text (-> source-report :pages first :text)}])
               candidate (fn [page] {:page page :reason "Whole-report guess" :image-description "Unverified description"})
               proposed {:summary (candidate 9) :photo (candidate 35) :map (candidate 16)
                         :descriptions (mapv #(hash-map :page % :image-description "Unverified description") [1 9 10 11 12])}
               content (vec (mapcat (fn [page] [{:type "input_text" :text (str page)}
                                                {:type "input_image" :image_url (str "test-page-" page)}]) (range 1 41)))]
           (-> (reduce (fn [previous mixed?]
                         (.then previous
                                (fn []
                                  (let [checked (atom [])
                                        request (fn [_ _ body check]
                                                  (let [input (-> body :input first :content)
                                                        page (js/parseInt (:text (first input)) 10)
                                                        finding (cond
                                                                  (= page 16) (assoc text-page-finding :has-location-map true :map-focus {:x 0.4 :y 0.25} :image-description "Location map.")
                                                                  (or (= page 19) (and mixed? (= page 35)))
                                                                  (assoc text-page-finding :has-incident-photo true :photo-overview-score 3 :photo-focus {:x 0.5 :y 0.3}
                                                                         :reason "Incident overview photo is visible, regardless of the chart."
                                                                         :image-description "Visible accident photo.")
                                                                  (= page 35) (assoc text-page-finding :reason "Chart only, no photograph." :image-description "A chart.")
                                                                  :else text-page-finding)]
                                                    (swap! checked conj page)
                                                    (is (= 2 (count input)))
                                                    (is (= "input_image" (:type (second input))))
                                                    (is (empty? (check finding)))
                                          ;; The verifier cannot move its finding to a different page.
                                                    (js/Promise.resolve (assoc finding :page 999))))]
                                    (-> (images/verify+ report limits content #{16 19 35} proposed request)
                                        (.then (fn [{:keys [candidates verifications]}]
                                                 (let [pages (images/resolve-pages report limits candidates)]
                                                   (is (= (if mixed? 35 19) (-> candidates :photo :page)))
                                                   (is (= "" (-> candidates :descriptions first :image-description)))
                                                   (is (= {:x 0.5 :y 0.3} (-> candidates :photo :focus)))
                                                   (is (= {:x 0.4 :y 0.25} (-> candidates :map :focus)))
                                                   (is (images/verified-selection? candidates pages verifications))
                                                   (is (not (images/verified-selection?
                                                             (assoc-in candidates [:photo :focus] {:x 0 :y 0}) pages verifications)))
                                                   (is (= (count @checked) (count (set @checked))))
                                                   (when mixed? (is (not (some #{19} @checked))))))))))))
                       (js/Promise.resolve nil) [false true])
               (.catch #(is false (str %)))
               (.finally done)))))

(deftest offline-end-to-end
  (async done
         (let [original-request ai/request+
               original-key (.. js/process -env -OPENAI_API_KEY)
               calls (atom []) reject-all? (atom false) root (atom nil)]
           (set! ai/request+
                 (fn [body _]
                   (swap! calls conj body)
                   (js/Promise.resolve
                    {:status "completed" :usage {:input_tokens 100 :output_tokens 50}
                     :output [{:content [{:type "output_text"
                                          :text (dry/json (if (= "verify_report_page" (-> body :text :format :name))
                                                            text-page-finding
                                                            {:summary (cond-> (first (:pages result))
                                                                        (or @reject-all? (= 1 (count @calls))) (assoc :page 99))
                                                             :photo nil :map nil :descriptions [{:page 1 :image-description ""}]}))}]}]})))
           (set! (.. js/process -env -OPENAI_API_KEY) "offline-key")
           (-> (fs/mkdtemp "/tmp/zugunfall-source-preview-test-")
               (.then (fn [directory]
                        (reset! root directory)
                        (fs/writeFile (str directory "/source.pdf")
                                      (sample-pdf ["1.1 Kurzbeschreibung des Ereignisses" "Ein Zug entgleiste in Teststadt."
                                                   "1.2 Folgen" "Niemand wurde verletzt." "1.3 Ursachen" "Andere Details."]))))
               (.then #(dry/preview-one+ (assoc report :pdf-path (str @root "/source.pdf")) limits {:out @root :model "stub"}))
               (.then (fn [saved]
                        (is (:ok? saved))
                        (is (= ["select_report_images" "select_report_images" "verify_report_page"] (mapv #(-> % :text :format :name) @calls)))
                        (is (= #{:summary :photo :map :descriptions} (set (keys (-> @calls first :text :format :schema :properties)))))
                        (is (= false (:store (first @calls))))
                        (is (str/includes? (-> @calls first :input first :content first :text) "PHYSICAL_PDF_PAGE=1"))
                        (is (= "input_image" (-> @calls first :input first :content second :type)))
                        (is (str/starts-with? (-> @calls first :input first :content second :image_url) "data:image/jpeg;base64,"))
                        (set! (.. js/process -env -OPENAI_API_KEY) "")
                        (dry/preview-one+ (assoc report :pdf-path (str @root "/source.pdf")) limits {:out @root :model "stub"})))
               (.then (fn [_] (is (= 3 (count @calls)))
                        (dry/read-json-if-present+ (str @root "/0123456789/comparison.json"))))
               (.then (fn [comparison]
                        (is (:cached? comparison)) (is (:accepted? comparison))
                        (is (= "source" (-> comparison :text-policy :mode)))
                        (is (every? :fits? (vals (-> comparison :ai :posts))))
                        (is (str/includes? (-> comparison :ai :posts :mastodon :text) "Niemand wurde verletzt."))
                        (is (= (-> comparison :ai :pages first :text) (-> comparison :ai :pages first :alt)))
                        (fs/readFile (str @root "/0123456789/index.html") "utf8")))
               (.then (fn [html]
                        (is (str/includes? html "Original extraction before font repair"))
                        (is (str/includes? html "copied programmatically"))
                        (is (str/includes? html "Full image alt text"))
                        (reset! reject-all? true)
                        (set! (.. js/process -env -OPENAI_API_KEY) "offline-key")
                        (-> (dry/preview-one+ (assoc report :pdf-path (str @root "/source.pdf")) limits {:out @root :model "stub" :refresh? true})
                            (.then #(is false "Invalid page selections must fail"))
                            (.catch (fn [error] (is (= 6 (count @calls))) (is (str/includes? (ex-message error) "/index.html")))))))
               (.then #(dry/read-json-if-present+ (str @root "/0123456789/comparison.json")))
               (.then #(is (= false (:accepted? %))))
               (.catch #(is false (str "Offline source preview: " %)))
               (.finally (fn [] (set! ai/request+ original-request)
                           (if original-key (set! (.. js/process -env -OPENAI_API_KEY) original-key)
                               (js-delete (.-env js/process) "OPENAI_API_KEY")) (done)))))))

(deftest preview-ai-summary-fallback
  (async done
         (let [original ai/request+ old-key (.. js/process -env -OPENAI_API_KEY) root (atom nil) calls (atom 0)]
           (set! (.. js/process -env -OPENAI_API_KEY) "offline-key")
           (set! ai/request+ (fn [body _]
                               (swap! calls inc)
                               (is (contains? #{"select_report_images" "verify_report_page"} (-> body :text :format :name)))
                               (js/Promise.resolve
                                {:status "completed" :output [{:content [{:type "output_text"
                                                                          :text (dry/json (if (= "verify_report_page" (-> body :text :format :name))
                                                                                            text-page-finding
                                                                                            {:summary {:page 1 :reason "Best incident overview" :image-description ""}
                                                                                             :photo nil :map nil :descriptions [{:page 1 :image-description ""}]}))}]}]})))
           (-> (fs/mkdtemp "/tmp/zugunfall-summary-image-fallback-")
               (.then (fn [folder] (reset! root folder) (fs/writeFile (str folder "/source.pdf") (sample-pdf))))
               (.then #(dry/preview-one+ (assoc report :pdf-path (str @root "/source.pdf")) limits {:out @root :model "stub"}))
               (.then #(dry/read-json-if-present+ (str @root "/0123456789/comparison.json")))
               (.then (fn [comparison]
                        (is (= [1] (mapv :page (-> comparison :ai :pages))))
                        (is (= false (:text-accepted? comparison)))
                        (is (= "missing-source" (-> comparison :text-policy :mode)))
                        (fs/readFile (str @root "/0123456789/index.html") "utf8")))
               (.then (fn [html]
                        (is (str/includes? html "headline for image review"))
                        (is (= 2 @calls))
                        ;; Both a preview cache and fresh generation must retain production's source-text requirement.
                        (reduce (fn [previous refresh?]
                                  (.then previous
                                         (fn []
                                           (-> (bot/prepare-report+ (assoc report :pdf-path (str @root "/source.pdf")) limits
                                                                    {:out @root :model "stub" :refresh? refresh? :image-review? true})
                                               (.then (fn [_] (is false "Missing source text must prevent production preparation")))
                                               (.catch (fn [error]
                                                         (is (str/includes? (ex-message error) "Event description section not found"))))))))
                                (js/Promise.resolve nil) [false true])))
               (.then (fn [] (is (= 2 @calls) "Production rejects missing source before any AI calls")))
               (.catch #(is false (str %)))
               (.finally (fn [] (set! ai/request+ original)
                           (if old-key (set! (.. js/process -env -OPENAI_API_KEY) old-key)
                               (js-delete (.-env js/process) "OPENAI_API_KEY")) (done)))))))

(deftest publishing-safety-and-alt-limits
  (is (thrown? js/Error (bsky/post-text report)))
  (is (thrown? js/Error (mastodon/toot-text report)))
  (let [posts (draft/draft-posts result report [] limits)
        ready (assoc report :posts posts :limits limits)]
    (is (= (-> posts :bluesky :text) (bsky/post-text ready)))
    (is (= (-> posts :mastodon :text) (mastodon/toot-text ready)))
    (is (thrown? js/Error (bsky/post-text (assoc-in ready [:posts :bluesky :text]
                                                    (str/join (repeat 301 "a"))))))
    (is (thrown? js/Error (mastodon/toot-text (assoc-in ready [:posts :mastodon :text]
                                                        (str/join (repeat 5001 "a")))))))
  (is (= "ä🚂…" (mastodon/shortened-description "ä🚂bc" 3)))
  (is (= "ä🚂b" (mastodon/shortened-description "ä🚂b" 3))))

(deftest offline-normal-publishing-flow
  (async done
         (let [originals [ai/request+ bot/prepare-report+ bot/env
                          bot/newest-report-ids-on-masto+ bot/newest-report-ids-on-bsky+
                          beu/fetch-reports+ beu/fetch-reports-details+ draft/mastodon-limits+
                          mastodon/upload-media+ mastodon/post+ bsky/upload-media+
                          bsky/get-access-token+ bsky/post+]
               original-key (.. js/process -env -OPENAI_API_KEY)
               root (atom nil)
               scenario (atom :valid)
               requests (atom [])
               uploads (atom [])
               published (atom [])
               description "Eine Karte zeigt die Unfallstelle."
               focal-point {:x 0.25 :y 0.3}
               test-limits (assoc limits :description_limit 40)]
           (set! (.. js/process -env -OPENAI_API_KEY) "offline-test-key")
           (set! bot/env "prod")
           (set! bot/newest-report-ids-on-masto+ #(js/Promise.resolve #{}))
           (set! bot/newest-report-ids-on-bsky+
                 #(js/Promise.resolve (if (= :masto-only @scenario) #{(:report-id report)} #{})))
           (set! beu/fetch-reports+
                 (fn [report-type]
                   (js/Promise.resolve
                    (if (and (= report-type "Untersuchungsbericht") (not= :none @scenario))
                      (cond-> [(assoc report :pdf-path (str @root "/source.pdf"))]
                        (= :invalid @scenario)
                        (conj (assoc report :pdf-path (str @root "/source.pdf")
                                     :report-id "[id:9876543210]")))
                      []))))
           (set! beu/fetch-reports-details+ #(js/Promise.resolve %))
           (set! draft/mastodon-limits+ (fn [_] (js/Promise.resolve test-limits)))
           (set! bot/prepare-report+
                 (fn [report limits opts]
                   ((nth originals 1) report limits (assoc opts :out @root :model "offline-model"))))
           (set! ai/request+
                 (fn [body _]
                   (swap! requests conj body)
                   (js/Promise.resolve
                    {:status "completed" :id "publishing-test" :usage {:input_tokens 1 :output_tokens 1}
                     :output [{:content [{:type "output_text"
                                          :text (dry/json
                                                 (if (= "verify_report_page" (-> body :text :format :name))
                                                   (assoc text-page-finding :has-location-map true :map-focus focal-point
                                                          :image-description description)
                                                   {:summary (cond-> (first (:pages result))
                                                               (= :invalid @scenario) (assoc :page 99))
                                                    :photo nil
                                                    :map {:page 1 :reason "Location map" :image-description description}
                                                    :descriptions [{:page 1 :image-description description}]}))}]}]})))
           (set! mastodon/upload-media+
                 (fn [media]
                   (swap! uploads conj [:mastodon media])
                   (js/Promise.resolve {:body {:id "masto-image"}})))
           (set! bsky/upload-media+
                 (fn [media]
                   (swap! uploads conj [:bluesky media])
                   (js/Promise.resolve {:body {:blob {:ref "bsky-image"}}})))
           (set! bsky/get-access-token+ #(js/Promise.resolve {:handle "offline.example"}))
           (set! mastodon/post+
                 (fn
                   ([path body]
                    (swap! published conj [:mastodon path body])
                    (js/Promise.resolve {}))
                   ([path body _]
                    (swap! published conj [:mastodon path body])
                    (js/Promise.resolve {}))))
           (set! bsky/post+
                 (fn
                   ([path body]
                    (swap! published conj [:bluesky path body])
                    (js/Promise.resolve {}))
                   ([path body _]
                    (swap! published conj [:bluesky path body])
                    (js/Promise.resolve {}))))
           (-> (fs/mkdtemp "/tmp/zugunfall-publishing-test-")
               (.then (fn [directory]
                        (reset! root directory)
                        (fs/writeFile (str directory "/source.pdf") (sample-pdf ["1.1 Kurzbeschreibung des Ereignisses" "Ein Zug entgleiste in Teststadt."
                                                                                 "1.2 Folgen" "Niemand wurde verletzt." "1.3 Ursachen" "Andere Details."]))))
               (.then bot/run+)
               (.then (fn [_]
                        (is (= 2 (count @requests)))
                        (is (= "select_report_images" (-> @requests first :text :format :name)))
                        (is (= #{:summary :photo :map :descriptions} (set (keys (-> @requests first :text :format :schema :properties)))))
                        (is (= "verify_report_page" (-> @requests second :text :format :name)))
                        (is (= "input_image" (-> @requests first :input first :content second :type)))
                        (is (= #{:mastodon :bluesky} (set (map first @published))))
                        (let [masto (->> @published (filter #(= :mastodon (first %))) first last)
                              bsky (get (->> @published (filter #(= :bluesky (first %))) first last) "record")
                              media (->> @uploads (filter #(= :mastodon (first %))) first last)]
                          (is (= (-> (draft/draft-posts (assoc result :mastodon (str (ai/headline report) "\n\nEin Zug entgleiste in Teststadt.\n\nNiemand wurde verletzt.")) report (-> (bot/add-post report) :post :tags) test-limits)
                                     :mastodon :text) (:status masto)))
                          (is (= ["masto-image"] (vec (:media_ids masto))))
                          (is (= (:bluesky result) (first (str/split (get bsky "text") #"\n"))))
                          (is (<= (draft/grapheme-count (get bsky "text")) 300))
                          (is (= (:report-overview-uri report)
                                 (-> bsky (get "facets") first :features first :uri)))
                          (is (= 40 (:description-limit media)))
                          (is (= focal-point (:focus media)))
                          (is (not (contains? (-> bsky (get "embed") (get "images") first) "focus")))
                          (is (str/starts-with? (:description media) (str "Bildbeschreibung:\n" description)))
                          (is (str/includes? (:description media) "Ein Zug entgleiste in Teststadt."))
                          (is (= (:description media) (-> bsky (get "embed") (get "images") first (get "alt")))))
                        (dry/read-json-if-present+ (str @root "/0123456789/publishing.json"))))
               (.then (fn [audit]
                        (is (-> audit :generation :posts :bluesky :fits?))
                        (is (= "source" (-> audit :generation :text-policy :mode)))
                        (is (-> audit :report :repair))
                        (is (= "roles" (-> audit :generation :image-policy)))
                        (is (= focal-point (-> audit :report :interesting-pages first :focus)))
                        (is (= ["cover" "summary" "map"] (-> audit :report :interesting-pages first :roles)))
                        (set! (.. js/process -env -OPENAI_API_KEY) "")
                        (dry/preview-one+ (assoc report :pdf-path (str @root "/source.pdf")) test-limits
                                          {:out @root :model "offline-model"})))
               (.then (fn [saved]
                        (is (:ok? saved))
                        (dry/read-json-if-present+ (str @root "/0123456789/comparison.json"))))
               (.then (fn [comparison]
                        (is (:cached? comparison) "Preview and production share the validated selection cache")
                        (is (= focal-point (-> comparison :ai :pages first :focus)))
                        (reset! published [])
                        (reset! uploads [])
                        (set! bot/env "dev")
                        (bot/run+)))
               (.then (fn [_]
                        (is (empty? @published))
                        (is (empty? @uploads))
                        (is (= 2 (count @requests)) "Non-prod run reuses the validated cache")
                        (set! bot/env "prod")
                        (reset! scenario :masto-only)
                        (bot/run+)))
               (.then (fn [_]
                        (is (= [:mastodon] (mapv first @published)) "Already-posted Bluesky report is skipped")
                        (reset! published [])
                        (reset! uploads [])
                        (reset! scenario :invalid)
                        (set! (.. js/process -env -OPENAI_API_KEY) "offline-test-key")
                        (-> (bot/run+)
                            (.then (fn [_] (is false "Invalid AI drafts must fail the publishing run")))
                            (.catch (fn [error]
                                      (is (str/includes? (ex-message error) "images rejected")))))))
               (.then (fn [_]
                        (is (= 5 (count @requests)))
                        (is (empty? @uploads))
                        (is (empty? @published))
                        (reset! scenario :none)
                        (bot/run+)))
               (.then (fn [_]
                        (is (= 5 (count @requests)) "No new reports means no AI request")
                        (is (empty? @uploads))
                        (is (empty? @published))))
               (.catch (fn [error] (is false (str "Publishing integration failed: " (.-stack error)))))
               (.finally (fn []
                           (let [[request prepare env masto-ids bsky-ids fetch-reports fetch-details get-limits
                                  masto-upload masto-post bsky-upload bsky-token bsky-post] originals]
                             (set! ai/request+ request)
                             (set! bot/prepare-report+ prepare)
                             (set! bot/env env)
                             (set! bot/newest-report-ids-on-masto+ masto-ids)
                             (set! bot/newest-report-ids-on-bsky+ bsky-ids)
                             (set! beu/fetch-reports+ fetch-reports)
                             (set! beu/fetch-reports-details+ fetch-details)
                             (set! draft/mastodon-limits+ get-limits)
                             (set! mastodon/upload-media+ masto-upload)
                             (set! mastodon/post+ masto-post)
                             (set! bsky/upload-media+ bsky-upload)
                             (set! bsky/get-access-token+ bsky-token)
                             (set! bsky/post+ bsky-post))
                           (if original-key
                             (set! (.. js/process -env -OPENAI_API_KEY) original-key)
                             (js-delete (.-env js/process) "OPENAI_API_KEY"))
                           (done)))))))

(deftest focus-coordinates-and-page-merging
  (is (= "-1,1" (focus/mastodon {:x 0 :y 0})))
  (is (= "1,-1" (focus/mastodon {:x 1 :y 1})))
  (is (= "0,0" (focus/mastodon {:x 0.5 :y 0.5})))
  (is (= "-0.5,0.5" (focus/mastodon {:x 0.25 :y 0.25})))
  (doseq [point [nil {} {:x -0.1 :y 0.5} {:x 0 :y 1.1} {:x js/NaN :y 0}
                 {:x js/Infinity :y 0} {:x "0.5" :y 0.5}]]
    (is (not (focus/valid? point)))
    (is (thrown? js/Error (focus/mastodon point))))
  (let [photo {:x 0.3 :y 0.2} location {:x 0.7 :y 0.6}
        candidate {:summary {:page 1 :reason "Summary" :image-description ""}
                   :photo {:page 1 :reason "Photo" :image-description "Photo." :focus photo}
                   :map {:page 1 :reason "Map" :image-description "Map." :focus location}
                   :descriptions []}
        merged (first (images/resolve-pages report limits candidate))
        selected (first (dry/selected-pages report [merged]))
        html (dry/page-html (assoc selected :image-path "report.pdf-1.png"))]
    (is (= ["cover" "summary" "photo" "map"] (:roles merged)))
    (is (= photo (:focus selected)))
    (is (= location (:focus (first (images/resolve-pages report limits (assoc candidate :photo nil))))))
    (is (str/includes? html "focus-marker"))
    (is (str/includes? html "left:30%;top:20%"))
    (is (str/includes? html "data-x=\"0.3\" data-y=\"0.2\"")))
  (is (empty? (images/verification-errors text-page-finding)))
  (is (seq (images/verification-errors (assoc text-page-finding :has-location-map true))))
  (is (seq (images/verification-errors (assoc text-page-finding :photo-focus {:x 0 :y 0})))))

(deftest focal-crop-centering-and-edge-clamping
  (let [html (dry/comparison-html {:report report :baseline {:pages []} :ai {:pages []}})
        script (second (re-find #"<script>(.*?)</script>" html))
        render (js/Function. "img" "ResizeObserver" (str script ";previewFocus(img);"))]
    (doseq [[y expected] [[0 "0px"] [0.5 "-337.5px"] [1 "-675px"]]]
      (let [img #js {:naturalWidth 600 :naturalHeight 900 :style #js {}
                     :parentElement #js {:clientWidth 600 :clientHeight 225 :dataset #js {:x "0.5" :y (str y)}}}]
        (render img (js/Function. "this.observe=()=>{};"))
        (is (= "600px" (.. img -style -width)))
        (is (= "900px" (.. img -style -height)))
        (is (= "0px" (.. img -style -left)))
        (is (= expected (.. img -style -top)))))))

(deftest mastodon-focal-point-upload
  (async done
         (let [original mastodon/multi-part-post+
               calls (atom [])]
           (set! mastodon/multi-part-post+
                 (fn [path parts _]
                   (swap! calls conj {:path path :parts parts})
                   (js/Promise.resolve {:ok? true :status 200 :body {:id "media"}})))
           (-> (mastodon/upload-screenshots+
                {:limits limits :interesting-pages
                 [{:image-path "photo.png" :text "Photo" :focus {:x 0.25 :y 0.25}}
                  {:image-path "text.png" :text "Text"}]})
               (.then (fn [uploaded]
                        (is (= ["media" "media"] (mapv :media-id (:interesting-pages uploaded))))
                        (is (= ["/api/v2/media" "/api/v2/media"] (mapv :path @calls)))
                        (is (= {:name "focus" :value "-0.5,0.5"}
                               (first (filter #(= "focus" (:name %)) (:parts (first @calls))))))
                        (is (not-any? #(= "focus" (:name %)) (:parts (second @calls))))
                        (is (thrown? js/Error (mastodon/upload-media+ {:path "bad.png" :description "Bad" :focus {:x 9 :y 0}})))))
               (.catch #(is false (str %)))
               (.finally (fn [] (set! mastodon/multi-part-post+ original) (done)))))))

(defmethod test/report [::test/default :end-run-tests] [summary]
  (when-not (test/successful? summary)
    (set! (.-exitCode js/process) 1)))

(defn main []
  (test/run-tests 'bot.dry-run-test))
