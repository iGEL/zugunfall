(ns bot.main
  (:require
   [bot.beu :as beu]))

(defn main []
  (-> (beu/fetch-reports+ "Zwischenbericht")
      (.then #(take 3 %))
      (.then beu/fetch-reports-details+)
      (.then #(doall (map prn %)))
      (.catch (fn [cause]
                (println (ex-message cause))))))
