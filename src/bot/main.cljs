(ns bot.main
  (:require
   [bot.beu :as beu]
   [bot.bsky :as bsky]
   [bot.date :as date]
   [bot.log :refer [log]]
   [bot.mastodon :as mastodon]
   [bot.pdf :as pdf]
   [clojure.string :as string]))

(def env (or (-> js/process .-env .-ENV)
             "dev"))

(defn newest-report-ids-on-masto+ []
  (-> (mastodon/get-toots+)
      (.then (fn [toots]
               (->> toots
                    (map mastodon/report-id)
                    (filter identity)
                    set)))))

(defn newest-report-ids-on-bsky+ []
  (-> (bsky/get-posts+)
      (.then (fn [posts]
               (->> posts
                    (map bsky/report-id)
                    (filter identity)
                    set)))))

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

(defn oldest-reports-to-post [[newest-ids-on-masto newest-ids-on-bsky final-reports intermediate-reports]]
  (->> (concat final-reports intermediate-reports)
       (sort-by #(-> % :report-date date/german->iso))
       (reduce (fn [prev {:keys [report-id] :as report}]
                 (let [post-to-masto? (-> newest-ids-on-masto (contains? report-id) not)
                       post-to-bsky? (-> newest-ids-on-bsky (contains? report-id) not)]
                   (if (or post-to-masto? post-to-bsky?)
                     (conj prev (assoc report
                                       :post-to-masto? post-to-masto?
                                       :post-to-bsky? post-to-bsky?))
                     prev)))
               [])))

(defn ^:export main []
  (-> (js/Promise.all [(newest-report-ids-on-masto+)
                       (newest-report-ids-on-bsky+)
                       (beu/fetch-reports+ "Untersuchungsbericht")
                       (beu/fetch-reports+ "Zwischenbericht")])
      (.then oldest-reports-to-post)
      (.then (fn [reports]
               (filter #(pos? (compare (-> % :report-date date/german->iso) "2025-01-01"))
                       reports)))
      (.then #(take 2 %))
      (.then beu/fetch-reports-details+)
      (.then #(js/Promise.all (map pdf/add-interesting-pages-with-screenshots+ %)))
      (.then #(map add-post %))
      (.then (fn [reports]
               (if (empty? reports)
                 (log "No new reports")
                 (let [masto-posts (->> reports (filter :post-to-masto?) seq)
                       bsky-posts (->> reports (filter :post-to-bsky?) seq)]
                   (log (string/join "\n"
                                     (cond-> []
                                       masto-posts
                                       (conj (str "Will publish " (count masto-posts) " toot(s) to Mastodon:\n"
                                                  (->> masto-posts (map :post-text) (string/join "\n\n"))))

                                       bsky-posts
                                       (conj (str "Will publish " (count bsky-posts) " post(s) to Bsky:\n"
                                                  (->> bsky-posts (map :post-text) (string/join "\n\n")))))))
                   (if (= env "prod")
                     (js/Promise.all (concat (map mastodon/publish-toot+ masto-posts)
                                             (map bsky/publish-post+ bsky-posts)))
                     (log (str "ENV is " env ", not prod. Publishing post is disabled.")))))))
      (.catch (fn [cause]
                (log (ex-message cause))))))
