(ns bot.mastodon
  (:require
   [bot.http :as http]
   [clojure.string :as string]))

(def instance-base-uri (-> js/process .-env .-INSTANCE_BASE_URI))
(def access-token (-> js/process .-env .-ACCESS_TOKEN))
(def description-max-length 1000)

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

(defn toot-first-link [toot]
  (re-find #"https?://[-a-zA-Z0-9@:%._+~#=]{1,256}\.[a-zA-Z0-9]{1,}/[-a-zA-Z0-9()@:%_+.~#?&/=]*" (toot-content {:text-only true} toot)))

(defn publish-toot+ [toot]
  (post+ "/api/v1/statuses" toot))

(defn shortened-description [description]
  (let [bytes (.encode (js/TextEncoder.) description)]
    (if (>= (.-length bytes) description-max-length)
      (let [shortened-bytes (.slice bytes 0 (- description-max-length 4))
            tdn (.decode (js/TextDecoder. "utf-8") shortened-bytes)]
        (str (.replace tdn #"\uFFFD" "") "…"))
      description)))

(defn upload-media+ [{:keys [path description content-type] :as info}]
  (-> (multi-part-post+ "/api/v2/media"
                        [{:name "description" :value (shortened-description description)}
                         {:name "file" :file path :content-type content-type}]
                        {:headers {:authorization (str "Bearer " access-token)}})
      (.then (fn [{:keys [status headers], :as response}]
               (if (= 429 status)
                 (let [wait-time (- (js/Date.parse (-> headers :x-ratelimit-reset first))
                                    (js/Date.now)
                                    -1000)]
                   (println (str "Got 429 Too Many Requests. Retrying in " wait-time "ms"))
                   (-> (js/Promise. (fn [resolve]
                                      (js/setTimeout resolve wait-time)))
                       (.then (partial upload-media+ info))))
                 response)))
      (.then http/ensure-ok+)))
