(ns bot.fetch
  (:require
   ["node-fetch$default" :as node-fetch]))

(defn ensure-200+ [{:keys [status uri] :as response}]
  (js/Promise. (fn [resolve reject]
                 (if (= 200 status)
                   (resolve response)
                   (reject (ex-info (str "Request to " uri " failed, expected 200 response, but got " status)
                                    response))))))

(defn fetch+ [uri]
  (-> (node-fetch uri)
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
