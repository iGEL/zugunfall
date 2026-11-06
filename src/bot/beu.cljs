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

(defn trim-text [value]
  (some-> value str/trim))

(defn extract-event-date [info]
  (or (->> info
           (re-find #"Ereignis vom:\s*([^,\n]+)")
           second
           trim-text)
      (throw (ex-info "Could not extract event date from BEU report details"
                      {:info info}))))

(defn add-report-id [report]
  (assoc report :report-id
         (str "[id:" (subs (->> (select-keys report [:report-type :report-date :event-location :event-type])
                                (sort-by (fn [[k _]] k)) pr-str sha1/sha1-sum) 0 10) "]")))

(defn overview-url [input]
  (let [url (js/URL. input)]
    (when-not (and (contains? #{"https:" "http:"} (.-protocol url))
                   (contains? #{"www.eisenbahn-unfalluntersuchung.de" "eisenbahn-unfalluntersuchung.de"}
                              (.-hostname url)))
      (throw (ex-info "--url must be a BEU report page or PDF URL" {})))
    (set! (.-pathname url) (str/replace (.-pathname url) #"(?i)\.pdf$" ".html"))
    (set! (.-search url) "")
    (set! (.-hash url) "")
    (.toString url)))

(defn extract-report-page [url {:keys [body]}]
  (let [document (parse body)
        main (.querySelector document "#main")
        info (some-> main (.querySelector ".info") .-text)
        title (some-> main (.querySelector "h1") .-text trim-text)
        download (some-> main (.querySelector "a.downloadLink") (.getAttribute "href"))
        report-type (cond (str/includes? url "/Zwischenberichte/") "Zwischenbericht"
                          (str/includes? url "/Untersuchungsberichte/") "Untersuchungsbericht")
        event-type (some->> info (re-find #"Thema:\s*(.+?),\s*Ereignis vom:") second trim-text)
        report-date (some->> info (re-find #"Publikation vom:\s*(\d{2}\.\d{2}\.\d{4})") second)]
    (when-not (and report-type info title download event-type report-date)
      (throw (ex-info "Could not read BEU report metadata; use its report detail page or PDF URL" {})))
    (add-report-id {:report-type report-type :report-date report-date
                    :event-type event-type :event-location title :event-date (extract-event-date info)
                    :report-overview-uri url
                    :report-pdf-uri (.toString (js/URL. (str/replace download "&amp;" "&") url))})))

(defn report-from-url+ [input]
  (let [url (overview-url input)]
    (-> (http/get+ url) (.then http/ensure-ok+) (.then #(extract-report-page url %)))))

(defn extract-search-results [report-type {:keys [body]}]
  (let [fragments (-> body
                      parse
                      (.querySelectorAll ".searchresult .row"))]
    (->> fragments
         (map (fn [fragment]
                {:report-type (trim-text report-type)
                 :report-date (->> (-> fragment (.querySelector "p") .-text)
                                   (re-find #"Publikation vom: (.+)")
                                   last
                                   trim-text)
                 :report-overview-uri (->> fragment
                                           (extract-uri "a")
                                           trim-text)
                 :report-pdf-uri (->> fragment
                                      (extract-uri "a.downloadLink")
                                      trim-text)
                 :event-location (-> fragment
                                     (.querySelector "a")
                                     .-firstChild
                                     .-text
                                     trim-text)
                 :event-type (-> fragment
                                 (.querySelectorAll "p")
                                 last
                                 .-text
                                 trim-text)}))
         (map add-report-id))))

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
                     info (some-> fragment
                                  (.querySelector ".info")
                                  .-text)
                     event-date (extract-event-date info)]
                 (assoc report :event-date event-date))))))

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
