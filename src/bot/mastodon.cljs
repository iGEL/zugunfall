(ns bot.mastodon
  (:require
   [bot.http :as http]
   [bot.log :refer [log]]
   [bot.post-draft :as draft]
   [clojure.string :as string]))

(def instance-base-uri (-> js/process .-env .-MASTO_BASE_URI))
(def access-token (-> js/process .-env .-MASTO_ACCESS_TOKEN))
(def visibility (or (-> js/process .-env .-MASTO_VISIBILITY)
                    "unlisted"))
(def description-max-length 1500)

(defn get+
  ([path]
   (get+ path {}))
  ([path opts]
   (-> (http/get+ (str instance-base-uri path)
                  (assoc-in opts [:headers :authorization] (str "Bearer " access-token)))
       (.then http/ensure-ok+)
       (.then http/parse-json+))))

(defn post+
  ([path body]
   (post+ path body {}))
  ([path body opts]
   (-> (http/post-json+ (str instance-base-uri path)
                        body
                        (assoc-in opts [:headers :authorization] (str "Bearer " access-token)))
       (.then http/ensure-ok+)
       (.then http/parse-json+))))

(defn multi-part-post+ [path parts opts]
  (-> (http/multi-part-post+ (str instance-base-uri path) parts opts)
      (.then http/parse-json+)))

(def account+
  (memoize
   (fn []
     (get+ "/api/v1/accounts/verify_credentials"))))

(def account-id+
  (memoize
   (fn []
     (-> (account+)
         (.then #(-> % :body :id))))))

(defn get-toots-page+ [{:keys [account-id prev max-pages]}]
  (-> (let [path (str "/api/v1/accounts/"
                      account-id
                      "/statuses?limit=40"
                      (when-not (empty? prev)
                        (str "&max_id=" (-> prev last :id))))]
        (get+ path))
      (.then (fn [{:keys [body]}]
               (if (or (empty? body)
                       (<= max-pages 1))
                 (concat prev body)
                 (get-toots-page+ {:account-id account-id
                                   :prev (concat prev body)
                                   :max-pages (dec max-pages)}))))))

(defn get-toots+ []
  (-> (account-id+)
      (.then (fn [account-id]
               (get-toots-page+ {:account-id account-id
                                 :max-pages 10})))))

(defn toot-content [{:keys [text-only]} {:keys [content]}]
  (if text-only
    (-> content
        (string/replace #"<br />" "\n")
        (string/replace #"<[^>]+>" ""))
    content))

(defn report-id [toot]
  (re-find #"\[id:[a-zA-Z0-9-_]{10}\]" (toot-content {:text-only true} toot)))

(defn shortened-description
  ([description] (shortened-description description description-max-length))
  ([description max-length]
   ;; Mastodon counts codepoints, not UTF-8 bytes. Never split a surrogate pair.
   (let [characters (js/Array.from description)]
     (if (> (.-length characters) max-length)
       (str (.join (.slice characters 0 (dec max-length)) "") "…")
       description))))

(defn upload-media+ [{:keys [path description content-type description-limit] :as report}]
  (-> (multi-part-post+ "/api/v2/media"
                        [{:name "description" :value (shortened-description description
                                                                            (or description-limit description-max-length))}
                         {:name "file" :file path :content-type content-type}]
                        {:headers {:authorization (str "Bearer " access-token)}})
      (.then (fn [{:keys [status headers], :as response}]
               (if (= 429 status)
                 (let [wait-time (- (js/Date.parse (-> headers :x-ratelimit-reset first))
                                    (js/Date.now)
                                    -1000)]
                   (log (str "Got 429 Too Many Requests. Retrying in " wait-time "ms"))
                   (-> (js/Promise. (fn [resolve]
                                      (js/setTimeout resolve wait-time)))
                       (.then (partial upload-media+ report))))
                 response)))
      (.then http/ensure-ok+)))

(defn upload-screenshots+ [{:keys [interesting-pages limits] :as report}]
  (-> (js/Promise.all (map (fn [{:keys [text alt image-path content-type]}]
                             (upload-media+ {:path image-path
                                             :description (or alt text)
                                             :description-limit (:description_limit limits)
                                             :content-type content-type}))
                           interesting-pages))
      (.then (fn [responses]
               (assoc report
                      :interesting-pages
                      (map-indexed (fn [idx response]
                                     (assoc (nth interesting-pages idx)
                                            :media-id
                                            (-> response :body :id)))
                                   responses))))))

(defn toot-text [report]
  (let [post (-> report :posts :mastodon)
        limits (:limits report)
        text (:text post)
        uri (:uri post)]
    (when-not (and (:fits? post) (string? text) (string? uri)
                   (string/includes? text uri)
                   (<= (+ (draft/grapheme-count (string/replace text uri ""))
                          (:characters_reserved_per_url limits))
                       (:max_characters limits)))
      (throw (ex-info "Refusing to publish an invalid Mastodon draft" {})))
    text))

(defn report->toot [{:keys [interesting-pages]
                     :as report}]
  {:status (toot-text report)
   :visibility visibility
   :language "de"
   :media_ids (->> interesting-pages
                   (map :media-id)
                   (filter identity))})

(defn publish-toot+ [report]
  (toot-text report)
  (-> (upload-screenshots+ report)
      (.then report->toot)
      (.then (fn [toot]
               (post+ "/api/v1/statuses" toot)))))
