(ns bot.dry-run-test
  (:require ["fs/promises" :as fs]
            [bot.ai :as ai]
            [bot.beu :as beu]
            [bot.bsky :as bsky]
            [bot.dry-run :as dry]
            [bot.main :as bot]
            [bot.mastodon :as mastodon]
            [bot.post-draft :as draft]
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
  {:bluesky "Untersuchungsbericht: In Teststadt entgleiste ein Zug. Niemand wurde verletzt."
   :mastodon "Untersuchungsbericht: In Teststadt entgleiste ein Zug. Niemand wurde verletzt."
   :pages [{:page 1 :reason "Anschauliche Skizze" :image-description ""}]
   :evidence [{:page 1 :claim "Ein Zug entgleiste in Teststadt."
               :quote "Ein Zug entgleiste in Teststadt."}]})

(def wire-result
  (assoc result :evidence [{:page 1 :claim "Ein Zug entgleiste in Teststadt."
                            :source-id "p1-s1"}]))

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

(deftest semantic-validation
  (is (= result (ai/validate-result result report limits)))
  (doseq [invalid [(assoc-in result [:pages 0 :page] 2)
                   (update result :pages #(conj % (first %)))
                   (assoc-in result [:pages 0 :reason] "")
                   (assoc-in result [:pages 0 :image-description] nil)
                   (assoc-in result [:evidence 0 :quote] "Invented quotation")
                   (assoc-in result [:evidence 0 :page] 99)
                   (assoc result :evidence [])
                   (assoc result :bluesky "See https://example.org")
                   (assoc result :bluesky "See HTTPS://example.org")
                   (assoc result :bluesky "#BEU")]]
    (is (thrown? js/Error (ai/validate-result invalid report limits)))))

(deftest report-type-in-first-five-words
  (doseq [report-type ["Untersuchungsbericht" "Zwischenbericht"]]
    (let [typed-report (assoc report :report-type report-type)
          typed-result (assoc result :bluesky (str report-type ": Ein Zug entgleiste.")
                              :mastodon (str report-type ": Ein Zug entgleiste."))]
      (is (= typed-result (ai/validate-result typed-result typed-report limits)))
      (doseq [opening [(str "Der " report-type " beschreibt das Ereignis.")
                       (str "Heute lesen wir den " report-type ".")]]
        (is (= (assoc typed-result :bluesky opening :mastodon opening)
               (ai/validate-result (assoc typed-result :bluesky opening :mastodon opening) typed-report limits))))
      (is (not (ai/report-type-near-start? (str "Heute lesen wir hier den " report-type ".") report-type)))
      (is (not (ai/report-type-near-start? (str report-type "erstattung zum Ereignis") report-type)))
      (doseq [platform [:bluesky :mastodon]]
        (is (some #(str/includes? % (str (name platform) ": summary must mention the report type"))
                  (ai/validation-errors (assoc typed-result platform "Ein Zug entgleiste.") typed-report limits)))))))

(deftest focused-source-correction-context
  (let [pdf-report (assoc report :pages [{:page 69 :text "Überschrift\n\nEine turnusmäßige Überwachung erfolgte nicht."}])
        invalid (assoc result :evidence [{:claim "Es gab keine turnusmäßige Überwachung."
                                          :page 69 :source-id "p69-s29"}])
        feedback (ai/correction-feedback invalid pdf-report limits ["unknown source passage p69-s29"])]
    (is (str/includes? feedback "p69-s29"))
    (is (str/includes? feedback "p69-s1"))
    (is (str/includes? feedback "p69-s2"))
    (is (str/includes? feedback "Eine turnusmäßige Überwachung erfolgte nicht."))
    (is (str/includes? feedback "Es gab keine turnusmäßige Überwachung."))
    (is (str/includes? feedback "Wiederhole die ungültigen IDs nicht"))
    (is (not (str/includes? (ai/correction-feedback result report limits ["length error"])
                            "Gezielte Quellenkorrekturen")))))

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

(deftest quotation-diagnostics-and-pdf-typography
  (let [pdf-report (assoc report :pages [{:page 1 :text "In Folge entstanden erhebliche\nSchäden an den betroﬀenen Wagen und der Sicherungs-\ntechnik."}
                                         {:page 2 :text "Niemand wurde verletzt."}])
        quoted (assoc-in result [:evidence 0 :quote]
                         "Schäden an den betroffenen Wagen und der Sicherungstechnik.")]
    (is (= quoted (ai/validate-result quoted pdf-report limits)))
    (is (= "Veröffentlichung" (ai/normalized "Veröﬀent\u00adlichung")))
    (let [wrong-page (assoc-in quoted [:evidence 0 :page] 2)
          errors (ai/validation-errors wrong-page pdf-report limits)]
      (is (= 1 (count errors)))
      (is (str/includes? (first errors) "evidence[0].quote"))
      (is (str/includes? (first errors) "matches physical PDF page(s) 1")))
    (is (seq (ai/validation-errors (assoc-in quoted [:evidence 0 :quote] "Invented claim") pdf-report limits)))
    ;; Lost letters in extracted text cannot be safely inferred by typography normalization.
    (is (seq (ai/validation-errors (assoc-in quoted [:evidence 0 :quote] "Rotterdam")
                                   (assoc report :pages [{:page 1 :text "Ro erdam"}]) limits))))
  (is (str/includes? (first (ai/validation-errors (assoc-in result [:pages 0 :page] 99) report limits))
                     "pages[0].page")))

(deftest source-references-handle-emmerich-extraction
  ;; Regression from the user's actual attempts: Poppler loses the 'tt' in Rotterdam,
  ;; while both model corrections quoted 'Roțerdam'. References avoid this transcription.
  (let [source "Am 19.09.2025 gegen 08:10 Uhr entgleiste der Güterzug DGS 47744 auf der Fahrt von\nEmmerich nach Ro erdam auf der Weiche 32W49 im Bahnhof Emmerich mit vier Fahrzeugen."
        emmerich-report (assoc report :pages [{:page 5 :text source}])
        proposal (assoc result :pages [{:page 5 :reason "Ereignisbeschreibung"}]
                        :evidence [{:page 5 :claim "Vier Fahrzeuge entgleisten."
                                    :source-id "p5-s1"}])
        materialized (ai/materialize-evidence proposal emmerich-report)]
    (is (= source (-> materialized :evidence first :quote)))
    (is (= materialized (ai/validate-result materialized emmerich-report limits)))
    (is (thrown? js/Error
                 (ai/validate-result (assoc proposal :evidence [{:page 5 :claim "Vier Fahrzeuge entgleisten."
                                                                 :quote (str/replace source "Ro erdam" "Roțerdam")}])
                                     emmerich-report limits)))
    (is (thrown? js/Error (ai/validate-result (assoc-in materialized [:evidence 0 :quote] "Invented text")
                                              emmerich-report limits)))
    (is (some #(str/includes? % "unknown source passage")
              (ai/validation-errors (ai/materialize-evidence (assoc-in proposal [:evidence 0 :source-id] "p5-s99")
                                                             emmerich-report)
                                    emmerich-report limits)))
    (is (some #(str/includes? % "source p5-s1 is on physical PDF page 5")
              (ai/validation-errors (assoc-in materialized [:evidence 0 :page] 1) emmerich-report limits)))
    (is (= ["p5-s1" "p5-s2"]
           (mapv :source-id (ai/source-passages (assoc report :pages [{:page 5 :text "First.\n\nSecond."}])))))))

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
  (is (not= (dry/cache-key report "model-a" limits "pdf")
            (dry/cache-key report "model-b" limits "pdf")))
  (is (not= (dry/cache-key report "model-a" limits "pdf")
            (dry/cache-key report "model-a" limits "changed-pdf")))
  (is (not= (dry/cache-key report "model-a" limits "pdf")
            (dry/cache-key report "model-a" (assoc limits :max_characters 500) "pdf")))
  (is (= "&lt;script&gt;&amp;&quot;&#39;" (dry/escape-html "<script>&\"'"))))

(defn sample-pdf []
  ;; A tiny real PDF exercises Poppler, image rendering, quotations and review artifacts.
  (let [stream "BT /F1 12 Tf 40 200 Td (Ein Zug entgleiste in Teststadt.) Tj 0 -20 Td (Niemand wurde verletzt.) Tj ET"
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
         "trailer\n<< /Size 6 /Root 1 0 R >>\nstartxref\n" (count body) "\n%%EOF\n")))

(deftest bounded-length-retries
  (async done
         (let [original-generate ai/generate+
               original-request ai/request+
               calls (atom [])
               rewrites (atom [])
               always-oversized? (atom false)
               oversized (assoc result :bluesky (str "Untersuchungsbericht: " (str/join (repeat 301 "a"))))]
           (set! ai/generate+
                 (fn [& _]
                   (swap! calls conj :full)
                   (js/Promise.resolve {:result oversized
                                        :usage {:input_tokens 100 :output_tokens 50}})))
           (set! ai/request+
                 (fn [body _]
                   (swap! calls conj :shorten)
                   (swap! rewrites conj body)
                   (js/Promise.resolve
                    {:status "completed" :usage {:input_tokens 10 :output_tokens 5}
                     :output [{:content [{:type "output_text"
                                          :text (dry/json {:bluesky (:bluesky (if @always-oversized? oversized result))})}]}]})))
           (-> (dry/generate-fitting+ report limits "offline-model" "unused" ["BEU"])
               (.then (fn [generated]
                        (is (= [:full :shorten] @calls))
                        (is (= [{:type "input_text"
                                 :text (str "Zu langer Text (" (draft/grapheme-count (:bluesky oversized))
                                            " Grapheme):\n" (:bluesky oversized))}]
                               (-> @rewrites first :input first :content)))
                        (is (= (:mastodon oversized) (-> generated :result :mastodon)))
                        (is (= (:pages oversized) (-> generated :result :pages)))
                        (is (= (:evidence oversized) (-> generated :result :evidence)))
                        (is (= 2 (count (:attempt-usages generated))))
                        (is (every? :fits? (vals (:posts generated))))
                        (reset! calls [])
                        (reset! always-oversized? true)
                        (-> (dry/generate-fitting+ report limits "offline-model" "unused" [])
                            (.then (fn [_] (is false "Oversized drafts must be rejected")))
                            (.catch (fn [error]
                                      (is (= [:full :shorten :shorten] @calls))
                                      (is (str/includes? (ex-message error) "exceed platform limits")))))))
               (.catch (fn [error] (is false (str "Retry test failed: " error))))
               (.finally (fn []
                           (set! ai/generate+ original-generate)
                           (set! ai/request+ original-request)
                           (done)))))))

(deftest offline-end-to-end
  (async done
         (let [original-request ai/request+
               original-key (.. js/process -env -OPENAI_API_KEY)
               calls (atom [])
               reject-all? (atom false)
               root (atom nil)]
           ;; Keep the stub installed until the asynchronous pipeline completes.
           (set! ai/request+
                 (fn [body _]
                   (swap! calls conj body)
                   (js/Promise.resolve
                    {:status "completed" :id "mock-response" :usage {:input_tokens 100 :output_tokens 50}
                     :output [{:content [{:type "output_text"
                                          :text (dry/json (if (or @reject-all? (= 1 (count @calls)))
                                                            (assoc-in wire-result [:evidence 0 :source-id] "p1-s99")
                                                            wire-result))}]}]})))
           (set! (.. js/process -env -OPENAI_API_KEY) "offline-test-key")
           (-> (fs/mkdtemp "/tmp/zugunfall-preview-test-")
               (.then (fn [directory]
                        (reset! root directory)
                        (fs/writeFile (str directory "/source.pdf") (sample-pdf))))
               (.then (fn []
                        (dry/preview-one+ (assoc report :pdf-path (str @root "/source.pdf"))
                                          limits {:out @root :model "offline-model"})))
               (.then (fn [saved]
                        (is (:ok? saved))
                        (is (= 2 (count @calls)))
                        (is (str/includes? (-> @calls second :input second :content first :text) "evidence[0].source-id"))
                        (is (str/includes? (-> @calls second :input second :content first :text) "p1-s1"))
                        (is (str/includes? (-> @calls second :input second :content first :text) "Ein Zug entgleiste in Teststadt."))
                        (is (= "input_file" (-> @calls second :input first :content first :type)))
                        (is (= false (:store (first @calls))))
                        (is (= "json_schema" (-> @calls first :text :format :type)))
                        (is (= "input_file" (-> @calls first :input first :content first :type)))
                        (set! (.. js/process -env -OPENAI_API_KEY) "")
                        ;; Cached run must succeed with no API key and make no second request.
                        (dry/preview-one+ (assoc report :pdf-path (str @root "/source.pdf"))
                                          limits {:out @root :model "offline-model"})))
               (.then (fn []
                        (is (= 2 (count @calls)))
                        (dry/read-json-if-present+ (str @root "/0123456789/comparison.json"))))
               (.then (fn [comparison]
                        (is (:cached? comparison))
                        (is (:accepted? comparison))
                        (is (= "p1-s1" (-> comparison :ai :evidence first :source-id)))
                        (is (str/includes? (-> comparison :ai :evidence first :quote) "Ein Zug entgleiste in Teststadt."))
                        (is (every? :fits? (vals (-> comparison :ai :posts))))
                        (is (= 1 (count (-> comparison :ai :pages))))
                        (is (= (-> comparison :ai :pages first :text) (-> comparison :ai :pages first :alt)))
                        (fs/readFile (str @root "/0123456789/index.html") "utf8")))
               (.then (fn [html]
                        (is (str/includes? html "Current bot"))
                        (is (str/includes? html "AI proposal"))
                        (is (str/includes? html "report.pdf-1."))
                        (fs/readdir (str @root "/0123456789"))))
               (.then (fn [files]
                        (is (= 2 (count (filter #(str/starts-with? % "attempt-") (js->clj files)))))
                        (reset! reject-all? true)
                        (set! (.. js/process -env -OPENAI_API_KEY) "offline-test-key")
                        (-> (dry/preview-one+ (assoc report :pdf-path (str @root "/source.pdf"))
                                              limits {:out @root :model "offline-model" :refresh? true})
                            (.then (fn [_] (is false "Three invalid results must fail the report")))
                            (.catch (fn [error]
                                      (is (= 5 (count @calls)))
                                      (is (str/includes? (ex-message error) "/index.html"))
                                      (is (str/includes? (ex-message error) "evidence[0].source-id")))))))
               (.then #(dry/read-json-if-present+ (str @root "/0123456789/comparison.json")))
               (.then (fn [comparison]
                        (is (= false (:accepted? comparison)))
                        (is (seq (:validation-errors comparison)))
                        (fs/readFile (str @root "/0123456789/index.html") "utf8")))
               (.then (fn [html]
                        (is (str/includes? html "AI proposal rejected"))
                        (is (str/includes? html "evidence[0].source-id"))))
               (.catch (fn [error] (is false (str "Offline integration failed: " error))))
               (.finally (fn []
                           (set! ai/request+ original-request)
                           (if original-key
                             (set! (.. js/process -env -OPENAI_API_KEY) original-key)
                             (js-delete (.-env js/process) "OPENAI_API_KEY"))
                           (done)))))))

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
               description "Eine Skizze zeigt die Unfallstelle."
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
                                                 (cond-> (assoc-in wire-result [:pages 0 :image-description] description)
                                                   (= :invalid @scenario)
                                                   (assoc-in [:evidence 0 :source-id] "p1-s99")))}]}]})))
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
                        (fs/writeFile (str directory "/source.pdf") (sample-pdf))))
               (.then bot/run+)
               (.then (fn [_]
                        (is (= 1 (count @requests)))
                        (is (= #{:mastodon :bluesky} (set (map first @published))))
                        (let [masto (->> @published (filter #(= :mastodon (first %))) first last)
                              bsky (get (->> @published (filter #(= :bluesky (first %))) first last) "record")
                              media (->> @uploads (filter #(= :mastodon (first %))) first last)]
                          (is (= (-> (draft/draft-posts result report (-> (bot/add-post report) :post :tags) test-limits)
                                     :mastodon :text) (:status masto)))
                          (is (= ["masto-image"] (vec (:media_ids masto))))
                          (is (= (:bluesky result) (first (str/split (get bsky "text") #"\n"))))
                          (is (<= (draft/grapheme-count (get bsky "text")) 300))
                          (is (= (:report-overview-uri report)
                                 (-> bsky (get "facets") first :features first :uri)))
                          (is (= 40 (:description-limit media)))
                          (is (str/starts-with? (:description media) (str "Bildbeschreibung:\n" description)))
                          (is (str/includes? (:description media) "Ein Zug entgleiste in Teststadt."))
                          (is (= (:description media) (-> bsky (get "embed") (get "images") first (get "alt")))))
                        (dry/read-json-if-present+ (str @root "/0123456789/publishing.json"))))
               (.then (fn [audit]
                        (is (-> audit :generation :posts :bluesky :fits?))
                        (reset! published [])
                        (reset! uploads [])
                        (set! bot/env "dev")
                        (bot/run+)))
               (.then (fn [_]
                        (is (empty? @published))
                        (is (empty? @uploads))
                        (is (= 1 (count @requests)) "Non-prod run reuses the validated cache")
                        (set! bot/env "prod")
                        (reset! scenario :masto-only)
                        (bot/run+)))
               (.then (fn [_]
                        (is (= [:mastodon] (mapv first @published)) "Already-posted Bluesky report is skipped")
                        (reset! published [])
                        (reset! uploads [])
                        (reset! scenario :invalid)
                        (-> (bot/run+)
                            (.then (fn [_] (is false "Invalid AI drafts must fail the publishing run")))
                            (.catch (fn [error]
                                      (is (str/includes? (ex-message error) "rejected after three attempts")))))))
               (.then (fn [_]
                        (is (= 4 (count @requests)))
                        (is (empty? @uploads))
                        (is (empty? @published))
                        (reset! scenario :none)
                        (bot/run+)))
               (.then (fn [_]
                        (is (= 4 (count @requests)) "No new reports means no AI request")
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

(defmethod test/report [::test/default :end-run-tests] [summary]
  (when-not (test/successful? summary)
    (set! (.-exitCode js/process) 1)))

(defn main []
  (test/run-tests 'bot.dry-run-test))
