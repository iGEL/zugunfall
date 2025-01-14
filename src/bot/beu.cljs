(ns bot.beu
  (:require
   ["node-html-parser" :refer [parse]]
   [bot.http :as http]
   [bot.sha1 :as sha1]
   [clojure.string :as str]))

(def report-type-uri {"Untersuchungsbericht" "https://www.eisenbahn-unfalluntersuchung.de/SiteGlobals/Forms/Suche/Untersuchungsberichtesuche/Untersuchungsberichtesuche_Formular.html?sortOrder=dateOfIssue_dt+desc"
                      "Zwischenbericht" "https://www.eisenbahn-unfalluntersuchung.de/SiteGlobals/Forms/Suche/Untersuchungsberichtesuche/Zwischenberichtesuche_Formular.html?sortOrder=dateOfIssue_dt+desc"})

(defn uri [path]
  (str "https://www.eisenbahn-unfalluntersuchung.de"
       (when-not (str/starts-with? path "/") "/")
       (str/replace path #";jsessionid=[^?]*" "")))

(defn extract-uri [query fragment]
  (-> fragment
      (.querySelector query)
      (.getAttribute "href")
      uri))

(defn extract-search-results [report-type {:keys [body]}]
  (let [fragments (-> body
                      parse
                      (.querySelectorAll ".searchresult .row"))]
    (->> fragments
         (map (fn [fragment]
                {:report-type (str/trim report-type)
                 :report-date (->> (-> fragment (.querySelector "p") .-text)
                                   (re-find #"Publikation vom: (.+)")
                                   last
                                   str/trim)
                 :report-overview-uri (->> fragment
                                           (extract-uri "a")
                                           str/trim)
                 :report-pdf-uri (->> fragment
                                      (extract-uri "a.downloadLink")
                                      str/trim)
                 :event-location (-> fragment
                                     (.querySelector "a")
                                     .-firstChild
                                     .-text
                                     str/trim)
                 :event-type (-> fragment
                                 (.querySelectorAll "p")
                                 last
                                 .-text
                                 str/trim)}))
         (map (fn [report]
                (let [report-id (str
                                 "[id:"
                                 (subs
                                  (->> (select-keys report [:report-type :report-date :event-location :event-type])
                                       (sort-by (fn [[k _]] k))
                                       pr-str
                                       sha1/sha1-sum)
                                  0 10)
                                 "]")]
                  (assoc report :report-id report-id)))))))

(defn has-report-details? [report]
  (contains? #{"Untersuchungsbericht" "Zwischenbericht"}
             (:report-type report)))

(defn fetch-one-report-detail+ [report]
  (-> (http/get+ (:report-overview-uri report))
      (.then http/ensure-ok+)
      (.then (fn [{:keys [body]}]
               (let [fragment (-> body
                                  parse
                                  (.querySelector "#main"))
                     info (-> fragment
                              (.querySelector ".info")
                              .-text)
                     [_ event-date] (re-find #"^Thema: .+, Ereignis vom: (.+), Publikation vom: .+$" info)]
                 (assoc report :event-date (str/trim event-date)))))))

(defn fetch-reports-details+ [reports]
  (js/Promise.all
   (map (fn [report]
          (if (has-report-details? report)
            (fetch-one-report-detail+ report)
            report))
        reports)))

(defn fetch-reports+ [report-type]
  (-> (http/get+ (report-type-uri report-type))
      (.then http/ensure-ok+)
      (.then (partial extract-search-results report-type))))
