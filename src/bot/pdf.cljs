(ns bot.pdf
  (:require
   ["child_process" :as child-process]
   ["fs/promises" :as fs]
   [bot.http :as http]
   [bot.mastodon :as mastodon]
   [clojure.string :as string]))

(defn exec+ [cmd & args]
  (js/Promise. (fn [resolve reject]
                 (child-process/execFile
                  cmd
                  (clj->js args)
                  (fn [error stdout stderr]
                    (if error
                      (reject error)
                      (resolve {:stdout stdout
                                :stderr stderr})))))))

(defn download-pdf+ [{:keys [report-pdf-uri] :as info}]
  (let [pdf-path (str "reports/" (random-uuid) ".pdf")]
    (-> (http/raw-request+ report-pdf-uri {:method "GET"})
        (.then #(.arrayBuffer %))
        (.then #(js/Buffer.from %))
        (.then #(fs/writeFile pdf-path %))
        (.then #(assoc info :pdf-path pdf-path)))))

(defn info+ [path]
  (-> (exec+ "/usr/bin/pdfinfo" path)
      (.then (fn [{:keys [stdout]}]
               (->> (string/split stdout #"\n")
                    (reduce (fn [prev line]
                              (let [[key val] (string/split line #":\s+" 2)]
                                (assoc prev key val)))
                            {}))))))

(defn add-page-count+ [{:keys [pdf-path] :as info}]
  (-> (info+ pdf-path)
      (.then (fn [pdf-info]
               (assoc info
                      :page-count
                      (-> pdf-info (get "Pages") (js/parseInt 10)))))))

(defn add-text-content+ [{:keys [page-count pdf-path] :as info}]
  (-> (js/Promise.all (->> (range 1 (inc page-count))
                           (map #(exec+ "/usr/bin/pdftotext" pdf-path "-" "-f" % "-l" %))))
      (.then (fn [results]
               (assoc info
                      :pages
                      (map-indexed (fn [idx {text :stdout}]
                                     {:page (inc idx)
                                      :text (string/trim text)})
                                   results))))))

(defn select-interesting-pages+ [{:keys [pages page-count] :as info}]
  (assoc info
         :interesting-pages
         (if (<= page-count 4)
           pages
           (let [{start-page :page} (or (->> pages
                                             (drop 3)
                                             (filter #(string/includes?
                                                       (:text %)
                                                       "Kurzbeschreibung des Ereignisses"))
                                             first)
                                        10000)]
             (->> pages
                  (filter #(or (= 1 (:page %))
                               (<= start-page (:page %))))
                  (take 4))))))

(defn add-screenshots+ [{:keys [interesting-pages pdf-path] :as info}]
  (let [interesting-pages (->> interesting-pages
                               (map (fn [{:keys [page] :as interesting-page}]
                                      (assoc interesting-page :image-path (str pdf-path "-" page ".png"))))
                               vec)]
    (-> (js/Promise.all (map (fn [{:keys [page image-path]}]
                               (exec+ "/usr/bin/pdftoppm"
                                      pdf-path
                                      (string/replace image-path #"\.png$" "")
                                      "-png"
                                      "-f"
                                      page
                                      "-singlefile"))
                             interesting-pages))
        (.then (fn [_]
                 (assoc info :interesting-pages interesting-pages))))))

(defn upload-screenshots+ [{:keys [interesting-pages] :as info}]
  (-> (js/Promise.all (map (fn [{:keys [text image-path]}]
                             (mastodon/upload-media+ {:path image-path
                                                      :description text
                                                      :content-type "image/png"}))
                           interesting-pages))
      (.then (fn [responses]
               (assoc info
                      :interesting-pages
                      (map-indexed (fn [idx response]
                                     (assoc (get interesting-pages idx)
                                            :media-id
                                            (-> response :body :id)))
                                   responses))))))

(defn add-interesting-pages-with-screenshots+ [info]
  (-> (download-pdf+ info)
      (.then add-page-count+)
      (.then add-text-content+)
      (.then select-interesting-pages+)
      (.then add-screenshots+)
      (.then upload-screenshots+)))
