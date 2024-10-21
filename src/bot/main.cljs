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

(defn newest-links-on-masto+ []
  (-> (mastodon/get-toots+)
      (.then (fn [toots]
               (->> toots
                    (map mastodon/toot-first-link)
                    (filter identity)
                    set)))))

(defn report->toot [{:keys [report-type event-type event-date event-location report-overview-uri interesting-pages]}]
  {:status (str report-type " über " event-type " am " event-date " in " event-location "\n"
                report-overview-uri "\n"
                "#" event-type " #BahnBubble #ZugBubble #BEU #Unfall #" report-type)
   :visibility visibility
   :language "de"
   :media_ids (->> interesting-pages
                   (map :media-id)
                   (filter identity))})

(defn reports-not-on-masto [newest-links-on-masto reports]
  (-> (reduce (fn [{:keys [newest-links-on-masto] :as prev} {:keys [report-overview-uri] :as report}]
                (if (or (contains? newest-links-on-masto report-overview-uri)
                        (empty? newest-links-on-masto))
                  (update prev :newest-links-on-masto disj report-overview-uri)
                  (update prev :reports-not-on-masto conj report)))
              {:newest-links-on-masto newest-links-on-masto
               :reports-not-on-masto []}
              (reverse reports))
      :reports-not-on-masto
      reverse))

(defn main []
  (-> (js/Promise.all [(newest-links-on-masto+)
                       (beu/fetch-reports+ "Untersuchungsbericht")
                       (beu/fetch-reports+ "Zwischenbericht")])
      (.then (fn [[newest-links-on-masto final-reports intermediate-reports]]
               (->> (concat final-reports intermediate-reports)
                    (sort-by #(-> % :report-date date/german->iso))
                    (reports-not-on-masto newest-links-on-masto))))
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
