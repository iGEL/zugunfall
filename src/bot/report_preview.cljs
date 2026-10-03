(ns bot.report-preview
  (:require ["fs/promises" :as fs]
            [bot.ai :as ai]
            [bot.http :as http]
            [bot.pdf :as pdf]
            [bot.post-draft :as draft]
            [bot.report :as bot]
            [bot.sha1 :as sha1]
            [clojure.string :as str]))

(defn json [value]
  (.stringify js/JSON (clj->js value) nil 2))

(defn write-json+ [path value]
  (fs/writeFile path (json value)))

(defn read-json-if-present+ [path]
  (-> (fs/readFile path "utf8")
      (.then #(js->clj (.parse js/JSON %) :keywordize-keys true))
      (.catch (fn [error]
                (if (= "ENOENT" (.-code error)) nil (throw error))))))

(defn prepare-pdf+ [report directory]
  (-> (fs/mkdir directory #js {:recursive true})
      (.then (fn []
               (if (:pdf-path report)
                 (-> (fs/copyFile (:pdf-path report) (str directory "/report.pdf"))
                     (.then #(assoc report :pdf-path (str directory "/report.pdf"))))
                 (let [path (str directory "/report.pdf")]
                   (-> (http/raw-request+ (:report-pdf-uri report) {:method "GET"})
                       (.then (fn [response]
                                (when-not (.-ok response)
                                  (throw (ex-info "Could not download report PDF" {})))
                                (.arrayBuffer response)))
                       (.then #(fs/writeFile path (js/Buffer.from %)))
                       (.then #(assoc report :pdf-path path)))))))
      (.then pdf/add-page-count+)
      (.then pdf/add-text-content+)
      (.then bot/add-post)))

(defn cache-key [report model limits pdf-base64]
  (sha1/sha1-sum (pr-str [ai/prompt-version ai/schema
                          (ai/instructions report limits)
                          model limits (:post report) pdf-base64])))

(defn generate-fitting+
  ([report limits model api-key tags]
   (generate-fitting+ report limits model api-key tags nil))
  ([report limits model api-key tags directory]
   (let [run-id (.now js/Date)]
     (letfn [(attempt [n feedback usages previous]
               (-> (if previous
                     (ai/shorten-bluesky+ (:result previous) report limits model api-key n)
                     (ai/generate+ report limits model api-key feedback))
                   (.then (fn [{:keys [result usage] :as generated}]
                            (let [validation-errors (ai/validation-errors result report limits)
                                  posts (when (empty? validation-errors)
                                          (draft/draft-posts result report tags limits))
                                  length-errors (for [[platform post] posts :when (not (:fits? post))]
                                                  (str (name platform) ": drafts exceed platform limits ("
                                                       (:length post) "/" (:max-length post)
                                                       " graphemes, " (:bytes post) " UTF-8 bytes). Verkürze den Text."))
                                  errors (into validation-errors length-errors)
                                  usages* (conj usages usage)
                                  generated* (assoc generated :posts posts :attempt-usages usages*
                                                    :validation-errors errors
                                                    :retry-feedback (when-not previous feedback))
                                  path (when directory (str directory "/attempt-" run-id "-" n ".json"))]
                              (-> (if path (write-json+ path generated*) (js/Promise.resolve nil))
                                  (.then (fn []
                                           (cond
                                             (empty? errors) generated*
                                             (< n 3)
                                             (do
                                               (js/console.warn (str "AI attempt " n "/3 rejected: "
                                                                     (str/join "; " errors) ". Requesting correction."))
                                               (attempt (inc n)
                                                        (ai/correction-feedback result report limits errors)
                                                        usages*
                                                        (when (and (empty? validation-errors)
                                                                   (not (-> posts :bluesky :fits?))
                                                                   (-> posts :mastodon :fits?))
                                                          generated*)))
                                             :else
                                             (throw (ex-info (str "AI draft rejected after three attempts: "
                                                                  (str/join "; " errors))
                                                             {:validation-errors errors :generated generated*})))))))))))]
       (attempt 1 nil [] nil)))))

(defn cached-generation+ [report limits opts directory]
  (-> (fs/readFile (:pdf-path report))
      (.then (fn [pdf]
               (let [key (cache-key report (:model opts) limits (.toString pdf "base64"))
                     path (str directory "/cache-" key ".json")]
                 (-> (if (:refresh? opts)
                       (js/Promise.resolve nil)
                       (read-json-if-present+ path))
                     (.then (fn [cached]
                              (if cached
                                (let [result (ai/validate-result (:result cached) report limits)
                                      posts (draft/draft-posts result report (-> report :post :tags) limits)]
                                  (when-not (every? :fits? (vals posts))
                                    (throw (ex-info "Cached drafts exceed current platform limits; use --refresh" {})))
                                  (assoc cached :posts posts :cached? true))
                                (-> (generate-fitting+ report limits (:model opts)
                                                       (.. js/process -env -OPENAI_API_KEY)
                                                       (-> report :post :tags) directory)
                                    (.then (fn [generated]
                                             (-> (write-json+ path (dissoc generated :posts))
                                                 (.then #(assoc generated :cached? false)))))))))))))))

(defn selected-pages [report selections]
  ;; Only the visual description comes from the model; retain every character of extracted text.
  ;; Preserve its order, including any explicitly justified departure from PDF page order.
  (mapv (fn [selection]
          (let [page (nth (:pages report) (dec (:page selection)))
                description (if (string? (:image-description selection))
                              (str/trim (:image-description selection)) "")
                alt (if (str/blank? description)
                      (:text page)
                      (str "Bildbeschreibung:\n" description "\n\nText der Seite:\n" (:text page)))]
            (merge page (select-keys selection [:page :reason])
                   {:image-description description :alt alt})))
        selections))
