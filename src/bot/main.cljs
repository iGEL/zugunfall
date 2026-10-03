(ns bot.main
  (:require
   [bot.ai :as ai]
   [bot.beu :as beu]
   [bot.bsky :as bsky]
   [bot.date :as date]
   [bot.log :refer [log]]
   [bot.mastodon :as mastodon]
   [bot.pdf :as pdf]
   [bot.post-draft :as draft]
   [bot.report :as report]
   [bot.report-preview :as preview]
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

(def capitalized-tag report/capitalized-tag)
(def add-post report/add-post)

(defn oldest-reports-to-post [[newest-ids-on-masto newest-ids-on-bsky final-reports intermediate-reports]]
  (->> (concat final-reports intermediate-reports)
       (sort-by #(-> % :report-date date/german->iso))
       (reduce (fn [prev {:keys [report-id] :as report}]
                 (let [post-to-masto? (and (-> newest-ids-on-masto (contains? report-id) not)
                                           (pos? (compare (-> report :report-date date/german->iso) "2025-01-01")))
                       post-to-bsky? (-> newest-ids-on-bsky (contains? report-id) not)]
                   (if (or post-to-masto? post-to-bsky?)
                     (conj prev (assoc report
                                       :post-to-masto? post-to-masto?
                                       :post-to-bsky? post-to-bsky?))
                     prev)))
               [])))

(defn prepare-report+ [report limits opts]
  (let [directory (str (:out opts) "/" (subs (:report-id report) 4 14))]
    (log (str "Preparing AI post for " (:report-id report)))
    (-> (preview/prepare-pdf+ report directory)
        (.then (fn [prepared]
                 (-> (preview/cached-generation+ prepared limits opts directory)
                     (.then (fn [{:keys [result posts] :as generated}]
                              (-> (pdf/add-screenshots+
                                   (assoc prepared :posts posts :limits limits
                                          :interesting-pages (preview/selected-pages prepared (:pages result))))
                                  (.then (fn [ready]
                                           (-> (preview/write-json+ (str directory "/publishing.json")
                                                                    {:report (dissoc ready :pages)
                                                                     :generation generated
                                                                     :model (:model opts)})
                                               (.then (constantly ready))))))))))))))

(defn prepare-reports+ [reports]
  (if (empty? reports)
    (js/Promise.resolve [])
    (let [opts {:out "reports/ai-preview"
                :model (or (.. js/process -env -OPENAI_MODEL) ai/default-model)}]
      (-> (draft/mastodon-limits+ (or mastodon/instance-base-uri "https://zug.network"))
          (.then (fn [limits]
                   ;; Prepare sequentially; no uploads or publishing until every draft is valid.
                   (reduce (fn [previous report]
                             (.then previous
                                    (fn [ready]
                                      (-> (prepare-report+ report limits opts)
                                          (.then #(conj ready %))))))
                           (js/Promise.resolve []) reports)))))))

(defn publish-reports+ [reports]
  (if (empty? reports)
    (log "No new reports")
    (let [masto-posts (->> reports (filter :post-to-masto?) seq)
          bsky-posts (->> reports (filter :post-to-bsky?) seq)]
      (when masto-posts
        (log (str "Will publish " (count masto-posts) " toot(s) to Mastodon:\n"
                  (->> masto-posts (map mastodon/toot-text) (string/join "\n\n")))))
      (when bsky-posts
        (log (str "Will publish " (count bsky-posts) " post(s) to Bsky:\n"
                  (->> bsky-posts (map bsky/post-text) (string/join "\n\n")))))
      (if (= env "prod")
        (js/Promise.all (concat (map mastodon/publish-toot+ masto-posts)
                                (map bsky/publish-post+ bsky-posts)))
        (log (str "ENV is " env ", not prod. Publishing post is disabled."))))))

(defn run+ []
  (-> (js/Promise.all [(newest-report-ids-on-masto+)
                       (newest-report-ids-on-bsky+)
                       (beu/fetch-reports+ "Untersuchungsbericht")
                       (beu/fetch-reports+ "Zwischenbericht")])
      (.then oldest-reports-to-post)
      (.then #(take 2 %))
      (.then beu/fetch-reports-details+)
      (.then prepare-reports+)
      (.then publish-reports+)))

(defn ^:export main []
  (-> (run+)
      (.catch (fn [cause]
                (log (or (ex-message cause) (.-message cause)))
                (set! (.-exitCode js/process) 1)))))
