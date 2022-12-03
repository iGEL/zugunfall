(ns bot.date
  (:require
   [clojure.string :as str]))

(defn german->iso [date]
  (->> (str/split date #"\.")
       reverse
       (str/join "-")))
