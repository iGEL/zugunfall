(ns bot.main
  (:require
   [bot.beu :as beu]
   [bot.date :as date]
   [bot.log :refer [log]]
   [bot.mastodon :as mastodon]
   [bot.pdf :as pdf]
   [clojure.string :as string]))

(def env (or (-> js/process .-env .-ENV)
             "dev"))
(def visibility (or (-> js/process .-env .-VISIBILITY)
                    "unlisted"))

(defn newest-report-ids-on-masto+ []
  (-> (mastodon/get-toots+)
      (.then (fn [toots]
               (->> toots
                    (map mastodon/report-id)
                    (filter identity)
                    set)))))

(defn report->toot [{:keys [report-type event-type event-date event-location report-overview-uri interesting-pages report-id]}]
  {:status (str report-type " über " event-type " am " event-date " in " event-location "\n"
                report-overview-uri "\n"
                "#" event-type " #BahnBubble #ZugBubble #BEU #Unfall #" report-type " " report-id)
   :visibility visibility
   :language "de"
   :media_ids (->> interesting-pages
                   (map :media-id)
                   (filter identity))})

(defn reports-not-on-masto [newest-ids-on-masto reports]
  (-> (reduce (fn [{:keys [newest-ids-on-masto] :as prev} {:keys [report-id] :as report}]
                (if (or (contains? newest-ids-on-masto report-id)
                        (empty? newest-ids-on-masto))
                  (update prev :newest-ids-on-masto disj report-id)
                  (update prev :reports-not-on-masto conj report)))
              {:newest-ids-on-masto newest-ids-on-masto
               :reports-not-on-masto []}
              (reverse reports))
      :reports-not-on-masto
      reverse))

(defn main []
  (-> (js/Promise.all [(newest-report-ids-on-masto+)
                       (beu/fetch-reports+ "Untersuchungsbericht")
                       (beu/fetch-reports+ "Zwischenbericht")])
      (.then (fn [[newest-ids-on-masto final-reports intermediate-reports]]
               (->> (concat final-reports intermediate-reports)
                    (sort-by #(-> % :report-date date/german->iso))
                    (reports-not-on-masto newest-ids-on-masto))))
      (.then (fn [reports]
               (filter #(pos? (compare (-> % :report-date date/german->iso) "2025-01-01"))
                       reports)))
      (.then #(take 2 %))
      (.then beu/fetch-reports-details+)
      (.then #(js/Promise.all (map pdf/add-interesting-pages-with-screenshots+ %)))
      (.then #(map report->toot %))
      (.then (fn [toots]
               (if (empty? toots)
                 (log "No new reports")
                 (log (str "Will publish " (count toots) " toot(s):\n" (->> toots (map :status) (string/join "\n\n")))))
               toots))
      (.then #(if (= env "prod")
                (js/Promise.all (map mastodon/publish-toot+ %))
                (log (str "ENV is " env ", not prod. Publishing toots is disabled."))))
      (.catch (fn [cause]
                (log (ex-message cause))))))
