(ns bot.sha1
  (:require
   ["crypto" :as crypto]
   [clojure.string :as str]))

(defn sha1-sum [input]
  (let [hash (.update (crypto/createHash "sha1") input "utf8")
        digest (.digest hash)]
    (-> (.toString digest "base64")
        (str/replace #"\+" "-")
        (str/replace #"/" "_")
        (str/replace #"=" ""))))
