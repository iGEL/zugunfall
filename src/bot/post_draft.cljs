(ns bot.post-draft
  (:require [bot.http :as http]
            [clojure.string :as str]))

(def bsky-max-graphemes 300)
(def bsky-max-bytes 3000)
(def link-label "BEU-Bericht")
(def segmenter (js/Intl.Segmenter. "de" #js {:granularity "grapheme"}))

(defn grapheme-count [text]
  (.-length (js/Array.from (.segment segmenter text))))

(defn byte-count [text]
  (.-length (.encode (js/TextEncoder.) text)))

(defn mastodon-limits+ [base-uri]
  (-> (http/get+ (str (str/replace base-uri #"/$" "") "/api/v2/instance"))
      (.then http/ensure-ok+)
      (.then http/parse-json+)
      (.then (fn [{{{:keys [statuses media_attachments]} :configuration} :body}]
               (when-not (every? #(and (integer? %) (pos? %))
                                 ((juxt :max_characters :characters_reserved_per_url
                                        :max_media_attachments) statuses))
                 (throw (ex-info "Mastodon returned invalid status limits" {})))
               (let [description-limit (or (:description_limit media_attachments) 1500)]
                 (when-not (and (integer? description-limit) (pos? description-limit))
                   (throw (ex-info "Mastodon returned invalid media description limit" {})))
                 (assoc statuses :description_limit description-limit))))))

(defn suffix [tags report-id]
  (str "\n" (str/join " " (concat (map #(str "#" %) tags) [report-id]))))

(defn compose [platform body report tags limits]
  (let [uri (:report-overview-uri report)
        label (if (= platform :bluesky) link-label uri)
        text (str body "\n" label (suffix tags (:report-id report)))
        length (if (= platform :bluesky)
                 (grapheme-count text)
                 (+ (grapheme-count (str body "\n" (suffix tags (:report-id report))))
                    (:characters_reserved_per_url limits)))
        max-length (if (= platform :bluesky) bsky-max-graphemes (:max_characters limits))
        byte-length (byte-count text)
        start (byte-count (str body "\n"))]
    (cond-> {:text text :uri uri :tags (vec tags)
             :length length :max-length max-length :bytes byte-length
             :fits? (and (<= length max-length)
                         (or (= platform :mastodon) (<= byte-length bsky-max-bytes)))}
      (= platform :bluesky)
      (assoc :facets (into [{:index {:byteStart start :byteEnd (+ start (byte-count label))}
                             :features [{:$type "app.bsky.richtext.facet#link" :uri uri}]}]
                           (loop [remaining tags
                                  offset (+ start (byte-count label) 1)
                                  result []]
                             (if-let [tag (first remaining)]
                               (let [end (+ offset (byte-count (str "#" tag)))]
                                 (recur (rest remaining) (inc end)
                                        (conj result {:index {:byteStart offset :byteEnd end}
                                                      :features [{:$type "app.bsky.richtext.facet#tag"
                                                                  :tag tag}]})))
                               result)))))))

(defn fit-post [platform body report tags limits]
  ;; Preserve the complete summary and drop tags before requesting a rewrite.
  (loop [remaining (vec tags)]
    (let [post (compose platform (str/trim body) report remaining limits)]
      (if (or (:fits? post) (empty? remaining))
        (assoc post :dropped-tags (vec (drop (count remaining) tags)))
        (recur (pop remaining))))))

(defn body-budget [platform report limits]
  (let [empty-post (compose platform "" report [] limits)]
    (- (:max-length empty-post) (:length empty-post))))

(defn draft-posts [result report tags limits]
  {:bluesky (fit-post :bluesky (:bluesky result) report tags limits)
   :mastodon (fit-post :mastodon (:mastodon result) report tags limits)})
