(ns bot.beu
  (:require
   ["node-html-parser" :refer [parse]]
   [bot.fetch :refer [ensure-200+ fetch+]]
   [clojure.string :as str]))

(def report-type-uri {"Untersuchungsbericht" "https://www.eisenbahn-unfalluntersuchung.de/SiteGlobals/Forms/Suche/Untersuchungsberichtesuche/Untersuchungsberichtesuche_Formular.html?sortOrder=dateOfIssue_dt+desc"
                      "Zwischenbericht" "https://www.eisenbahn-unfalluntersuchung.de/SiteGlobals/Forms/Suche/Untersuchungsberichtesuche/Zwischenberichtesuche_Formular.html?sortOrder=dateOfIssue_dt+desc"})

(defn uri [path]
  (str "https://www.eisenbahn-unfalluntersuchung.de"
       (when-not (str/starts-with? path "/") "/")
       (str/replace path #";jsessionid=[^?]*" "")))

(defn german->iso-date [date]
  (->> (str/split date #"\.")
       reverse
       (str/join "-")))

(defn extract-uri [query fragment]
  (-> fragment
      (.querySelector query)
      (.getAttribute "href")
      uri))

(defn extract-search-results [report-type {:keys [body]}]
  (let [fragments (-> body
                      parse
                      (.querySelectorAll ".searchresult .row"))]
    (map (fn [fragment]
           {:report-type report-type
            :report-overview-uri (extract-uri "a" fragment)
            :report-pdf-uri (extract-uri "a.downloadLink" fragment)})
         fragments)))

(defn fetch-reports-details+ [reports]
  (js/Promise.all
   (map (fn [report]
          (-> (fetch+ (:report-overview-uri report))
              (.then ensure-200+)
              (.then (fn [{:keys [body]}]
                       (let [fragment (-> body
                                          parse
                                          (.querySelector "#main"))
                             info (-> fragment
                                      (.querySelector ".info")
                                      .-text)
                             [_ event-type event-date publication-date] (re-find #"^Thema: (.+), Ereignis vom: (.+), Publikation vom: (.+)$" info)]
                         (merge report {:event-type event-type
                                        :event-date event-date
                                        :publication-date publication-date}))))))
        reports)))

(defn fetch-reports+ [report-type]
  (-> (fetch+ (report-type-uri report-type))
      (.then ensure-200+)
      (.then (partial extract-search-results report-type))))
