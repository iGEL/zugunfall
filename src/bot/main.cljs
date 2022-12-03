(ns bot.main
  (:require
   [bot.beu :as beu]
   [bot.date :as date]))

(defn main []
  (-> (js/Promise.all [(beu/fetch-reports+ "Untersuchungsbericht")
                       (beu/fetch-reports+ "Zwischenbericht")])
      (.then (fn [[final-reports intermediate-reports]]
               (->> (concat final-reports intermediate-reports)
                    (sort-by #(-> % :report-date date/german->iso))
                    reverse)))
      (.then #(take 10 %))
      (.then beu/fetch-reports-details+)
      (.then #(doall (map prn %)))
      (.catch (fn [cause]
                (println (ex-message cause))))))
