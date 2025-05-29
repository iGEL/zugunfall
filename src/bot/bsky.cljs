(ns bot.bsky
  (:require
   ["fs/promises" :as fs]
   [bot.dub :as dub]
   [bot.http :as http]
   [bot.log :refer [log]]
   [clojure.string :as str]))

(def base-uri (or (-> js/process .-env .-BSKY_BASE_URI)
                  "https://bsky.social"))
(def token-file (or (-> js/process .-env .-BSKY_TOKEN_FILE)
                    "bsky.token"))

(def get-access-token+
  (memoize
   (fn []
     (log "Refreshing bsky access token")
     (-> (fs/readFile token-file "utf-8")
         (.then str/trim)
         (.then (fn [refresh-token]
                  (http/post+ (str base-uri "/xrpc/com.atproto.server.refreshSession")
                              nil
                              {:headers {:authorization (str "Bearer " refresh-token)}})))
         (.then http/ensure-ok+)
         (.then http/parse-json+)
         (.then (fn [{{access-token :accessJwt
                       refresh-token :refreshJwt
                       handle :handle} :body}]
                  {:access-token access-token
                   :refresh-token refresh-token
                   :handle handle}))
         (.then (fn [{:keys [refresh-token] :as info}]
                  (-> (fs/writeFile token-file refresh-token)
                      (.then (constantly (dissoc info :refresh-token))))))
         (.catch (fn [error]
                   (js/console.error error)
                   (js/Promise.reject error)))))))

(defn get+
  ([path]
   (get+ path {}))
  ([path opts]
   (-> (get-access-token+)
       (.then (fn [{:keys [access-token]}]
                (http/get+ (str base-uri path)
                           (assoc-in opts [:headers :authorization] (str "Bearer " access-token)))))
       (.then http/ensure-ok+)
       (.then http/parse-json+))))

(defn post+
  ([path body]
   (post+ path body {}))
  ([path body opts]
   (-> (get-access-token+)
       (.then (fn [{:keys [access-token]}]
                (http/post-json+ (str base-uri path)
                                 body
                                 (assoc-in opts [:headers :authorization] (str "Bearer " access-token)))))
       (.then http/ensure-ok+)
       (.then http/parse-json+))))

(defn upload-media+ [{:keys [path content-type]}]
  (-> (js/Promise.all [(get-access-token+)
                       (fs/readFile path)])
      (.then (fn [[{:keys [access-token]} buffer]]
               (http/post+ (str base-uri "/xrpc/com.atproto.repo.uploadBlob")
                           buffer
                           {:headers {:content-type content-type
                                      :authorization (str "Bearer " access-token)}})))
      (.then http/ensure-ok+)
      (.then http/parse-json+)))

(defn- get-post-pages+ [{:keys [actor cursor max-pages prev]}]
  (-> (get+ (str "/xrpc/app.bsky.feed.getAuthorFeed"
                 "?actor=" actor
                 (when cursor
                   (str "&cursor=" cursor))
                 "&filter=posts_no_replies&limit=100"))
      (.then (fn [{{:keys [feed cursor]} :body}]
               (if (or (not cursor)
                       (<= max-pages 1))
                 (concat prev feed)
                 (get-post-pages+ {:actor actor
                                   :prev (concat prev feed)
                                   :cursor cursor
                                   :max-pages (dec max-pages)}))))))

(defn get-posts+ []
  (-> (get-access-token+)
      (.then (fn [{:keys [handle]}]
               (get-post-pages+ {:actor handle
                                 :max-pages 4})))))

(defn report-id [post]
  (re-find #"\[id:[a-zA-Z0-9-_]{10}\]" (-> post :post :record :text)))

(defn upload-screenshots+ [{:keys [interesting-pages] :as report}]
  (-> (js/Promise.all (map (fn [{:keys [image-path content-type]}]
                             (upload-media+ {:path image-path
                                             :content-type content-type}))
                           interesting-pages))
      (.then (fn [results]
               (assoc report
                      :interesting-pages (map-indexed (fn [idx {{:keys [blob]} :body}]
                                                        (assoc (nth interesting-pages idx)
                                                               :image blob))
                                                      results))))))

(defn- byte-length [text]
  (->> text
       (.encode (js/TextEncoder.))
       .-length))

(defn post-text [{:keys [report-id]
                  {:keys [title tags uri]} :post}]
  (str title "\n" uri "\n" (str/join " " (map #(str "#" %) tags)) " " report-id))

(defn post-text-with-shortened-link+ [{{:keys [uri]} :post
                                       :as report}]
  (let [text-with-full-uri (post-text report)]
    (if (< (count text-with-full-uri) 300)
      (js/Promise.resolve {:text text-with-full-uri
                           :uri uri})
      (-> (dub/create-link+ {:uri uri})
          (.then (fn [short-uri]
                   {:text (post-text (assoc-in report [:post :uri] short-uri))
                    :uri short-uri}))))))

(defn report->post [{:keys [interesting-pages]
                     {:keys [title tags]} :post
                     :as report}]
  (-> (js/Promise.all [(get-access-token+)
                       (post-text-with-shortened-link+ report)])
      (.then (fn [[{:keys [handle]} {:keys [text uri]}]]
               (let [facets (reduce
                             (fn [prev tag]
                               (let [byte-start (-> prev last :index :byteEnd inc)]
                                 (conj prev
                                       {:index {:byteStart byte-start
                                                :byteEnd (+ byte-start (byte-length tag) 1)}
                                        :features [{"$type" "app.bsky.richtext.facet#tag"
                                                    "tag" tag}]})))
                             [{:index {:byteStart (byte-length title)
                                       :byteEnd (+ (byte-length title)
                                                   (byte-length uri)
                                                   1)}
                               :features [{"$type" "app.bsky.richtext.facet#link"
                                           "uri" uri}]}]
                             tags)]
                 {"repo" handle
                  "collection" "app.bsky.feed.post"
                  "record" {"$type" "app.bsky.feed.post"
                            "text" text
                            "facets" facets
                            "langs" ["de"]
                            "createdAt" (-> (js/Date.) .toISOString)
                            "embed" {"$type" "app.bsky.embed.images"
                                     "images" (map (fn [{:keys [image text]}]
                                                     {"alt" text
                                                      "image" image})
                                                   interesting-pages)}}})))))

(defn publish-post+ [report]
  (-> (upload-screenshots+ report)
      (.then report->post)
      (.then (fn [post]
               (post+ "/xrpc/com.atproto.repo.createRecord" post)))))
