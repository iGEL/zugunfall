(ns bot.dub
  "Management for the dub link shortening service"
  (:require
   [bot.http :as http]))

(def base-uri (or (-> js/process .-env .-DUB_BASE_URI)
                  "https://api.dub.co"))
(def api-key (-> js/process .-env .-DUB_API_KEY))

(defn post+
  ([path body]
   (post+ path body {}))
  ([path body opts]
   (-> (http/post-json+ (str base-uri path)
                        body
                        (assoc-in opts [:headers :authorization] (str "Bearer " api-key)))
       (.then http/ensure-ok+)
       (.then http/parse-json+))))

(defn create-link+ [{:keys [uri]}]
  (-> (post+ "/links" {:url uri})
      (.then #(-> % :body :shortLink))))
