(ns bot.mastodon
  (:require
   [bot.http :as http]
   [clojure.string :as string]))

(def instance-base-uri (-> js/process .-env .-INSTANCE_BASE_URI))
(def access-token (-> js/process .-env .-ACCESS_TOKEN))

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

(def account+
  (memoize
   (fn []
     (get+ "/api/v1/accounts/verify_credentials"))))

(def account-id+
  (memoize
   (fn []
     (-> (account+)
         (.then #(-> % :body :id))))))

(defn get-statuses+ []
  (-> (account-id+)
      (.then (fn [account-id]
               (get+ (str "/api/v1/accounts/" account-id "/statuses"))))
      (.then #(:body %))))

(defn status-content [{:keys [text-only]} {:keys [content]}]
  (if text-only
    (-> content
        (string/replace #"<br />" "\n")
        (string/replace #"<[^>]+>" ""))
    content))

(defn status-first-link [status]
  (re-find #"https?://[-a-zA-Z0-9@:%._+~#=]{1,256}\.[a-zA-Z0-9]{1,}/[-a-zA-Z0-9()@:%_+.~#?&/=]*" (status-content {:text-only true} status)))

(defn post-status+ [status]
  (post+ "/api/v1/statuses" status))
