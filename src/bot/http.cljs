(ns bot.http
  (:require
   ["node-fetch$default" :as node-fetch]
   [clojure.string :as string]))

(defn ensure-ok+ [{:keys [ok? status uri] :as response}]
  (js/Promise. (fn [resolve reject]
                 (if ok?
                   (resolve response)
                   (reject (ex-info (str "Request to " uri " failed, expected 200-299 response, but got " status "\n" (pr-str response))
                                    response))))))

(defn parse-json+ [{:keys [body]
                    {:keys [content-type]} :headers
                    :as response}]
  (if (-> content-type first string/lower-case (string/starts-with? "application/json"))
    (let [json (.parse js/JSON body)]
      (assoc response :body (js->clj json :keywordize-keys true)))
    response))

(defn request+ [uri opts]
  (-> (node-fetch uri (clj->js opts))
      (.then (fn [response]
               (-> (js/Promise.all [{:uri uri
                                     :status (.-status response)
                                     :ok? (.-ok response)
                                     :headers (-> response
                                                  .-headers
                                                  .raw
                                                  (js->clj :keywordize-keys true))}
                                    (.text response)])
                   (.then (fn [[response-map body]]
                            (assoc response-map :body body))))))))

(defn get+
  ([uri]
   (get+ uri {}))
  ([uri opts]
   (request+ uri (assoc opts :method "GET"))))

(defn post+
  ([uri body]
   (post+ uri body {}))
  ([uri body opts]
   (prn (assoc opts
               :method "POST"
               :body body))
   (request+ uri (assoc opts
                        :method "POST"
                        :body body))))

(defn post-json+
  ([uri body]
   (post-json+ uri body {}))
  ([uri body opts]
   (post+ uri
          (.stringify js/JSON (clj->js body))
          (assoc-in opts [:headers :content-type] "application/json"))))
