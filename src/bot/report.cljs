(ns bot.report
  (:require [clojure.string :as string]))

(defn capitalized-tag [tag]
  (->> (string/split tag #" ")
       (map string/capitalize)
       (string/join)))

(defn add-post [{:keys [report-type event-type event-date event-location
                        report-overview-uri]
                 :as report}]
  (assoc report :post
         {:title (str report-type " über " event-type " am " event-date " in " event-location)
          :uri report-overview-uri
          :tags [(capitalized-tag event-type)
                 "BahnBubble"
                 "ZugBubble"
                 "BEU"
                 "Unfall"
                 (capitalized-tag report-type)]}))
