(ns bot.ai
  (:require [bot.http :as http]
            [bot.report :as report]
            [clojure.string :as str]))

(def default-model "gpt-4.1-mini")

(defn headline [metadata]
  (-> metadata report/add-post :post :title))

(defn object-schema [properties]
  {:type "object" :properties properties
   :required (vec (keys properties)) :additionalProperties false})

(defn parse-response [response]
  (when-not (= "completed" (:status response))
    (throw (ex-info (str "OpenAI response did not complete: " (:status response)) {})))
  (let [content (mapcat :content (:output response))]
    (when (some #(= "refusal" (:type %)) content)
      (throw (ex-info "OpenAI refused this request" {})))
    (let [text (->> content (filter #(= "output_text" (:type %))) (map :text) (str/join))]
      (when (str/blank? text)
        (throw (ex-info "OpenAI returned no structured result" {})))
      (js->clj (.parse js/JSON text) :keywordize-keys true))))

(defn request+ [body api-key]
  ;; Do not use ensure-ok+: its error includes the entire response.
  (-> (http/post-json+ "https://api.openai.com/v1/responses" body
                       {:headers {:authorization (str "Bearer " api-key)}
                        :signal (js/AbortSignal.timeout 180000)})
      (.then (fn [{:keys [ok? status] :as response}]
               (when-not ok?
                 (throw (ex-info (str "OpenAI request failed (HTTP " status "). Check the API key, model access and credits.") {})))
               (:body (http/parse-json+ response))))))
