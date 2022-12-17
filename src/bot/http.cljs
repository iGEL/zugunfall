(ns bot.http
  (:require
   ["node-fetch" :as node-fetch]
   ["node-fetch$default" :as fetch]
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

(defn raw-request+
  ([uri]
   (raw-request+ uri {}))
  ([uri opts]
   (fetch uri (clj->js opts))))

(defn request+ [uri opts]
  (-> (raw-request+ uri (clj->js opts))
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

(defn multi-part-post+
  ([uri parts]
   (multi-part-post+ uri parts {}))
  ([uri parts opts]
   (let [form-data (node-fetch/FormData.)]
     (doseq [{:keys [name file value content-type]} parts]
       (if file
         (.set form-data name (node-fetch/fileFromSync file content-type))
         (.set form-data name value)))
     (post+ uri form-data opts))))
