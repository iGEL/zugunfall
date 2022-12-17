(ns bot.main
  (:require
   [bot.beu :as beu]
   [bot.date :as date]
   [bot.mastodon :as mastodon]
   [bot.pdf :as pdf]))

(def visibility (or (-> js/process .-env .-VISIBILITY)
                    "unlisted"))

(defn newest-link-on-masto+ []
  (-> (mastodon/get-statuses+)
      (.then (fn [statuses]
               (->> statuses
                    (map mastodon/status-first-link)
                    (filter identity)
                    first)))))

(defn report->status [{:keys [report-type event-type event-date event-location report-overview-uri interesting-pages]}]
  {:status (str report-type " über " event-type " am " event-date " in " event-location "\n" report-overview-uri)
   :visibility visibility
   :language "de"
   :media_ids (->> interesting-pages
                   (map :media-id)
                   (filter identity))})

(defn main []
  (-> (js/Promise.all [(newest-link-on-masto+)
                       (beu/fetch-reports+ "Untersuchungsbericht")
                       (beu/fetch-reports+ "Zwischenbericht")])
      (.then (fn [[newest-link final-reports intermediate-reports]]
               (->> (concat final-reports intermediate-reports)
                    (sort-by #(-> % :report-date date/german->iso))
                    reverse
                    (take-while #(not= newest-link (:report-overview-uri %)))
                    reverse)))
      (.then beu/fetch-reports-details+)
      (.then #(js/Promise.all (map pdf/add-interesting-pages-with-screenshots+ %)))
      (.then #(->> %
                   (map report->status)
                   (map mastodon/post-status+)
                   js/Promise.all))
      (.catch (fn [cause]
                (println (ex-message cause))))))
