(ns bot.log)

(defn log [msg]
  (let [m (if (map? msg) msg {:msg msg})]
    (-> (cond-> m
          (not (:date m)) (assoc :date (-> (js/Date.) (.toISOString))))
        (clj->js)
        (js/JSON.stringify)
        (println))))
