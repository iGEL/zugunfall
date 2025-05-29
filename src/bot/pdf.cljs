(ns bot.pdf
  (:require
   ["child_process" :as child-process]
   ["fs/promises" :as fs]
   [bot.http :as http]
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

(defn download-pdf+ [{:keys [report-pdf-uri] :as report}]
  (let [pdf-path (str "reports/" (random-uuid) ".pdf")]
    (-> (http/raw-request+ report-pdf-uri {:method "GET"})
        (.then #(.arrayBuffer %))
        (.then #(js/Buffer.from %))
        (.then #(fs/writeFile pdf-path %))
        (.then #(assoc report :pdf-path pdf-path)))))

(defn info+ [path]
  (-> (exec+ "/usr/bin/pdfinfo" path)
      (.then (fn [{:keys [stdout]}]
               (->> (string/split stdout #"\n")
                    (reduce (fn [prev line]
                              (let [[key val] (string/split line #":\s+" 2)]
                                (assoc prev key val)))
                            {}))))))

(defn add-page-count+ [{:keys [pdf-path] :as report}]
  (-> (info+ pdf-path)
      (.then (fn [pdf-info]
               (assoc report
                      :page-count
                      (-> pdf-info (get "Pages") (js/parseInt 10)))))))

(defn add-text-content+ [{:keys [page-count pdf-path] :as report}]
  (-> (js/Promise.all (->> (range 1 (inc page-count))
                           (map #(exec+ "/usr/bin/pdftotext" pdf-path "-" "-f" % "-l" %))))
      (.then (fn [results]
               (assoc report
                      :pages
                      (map-indexed (fn [idx {text :stdout}]
                                     {:page (inc idx)
                                      :text (string/trim text)})
                                   results))))))

(defn select-interesting-pages+ [{:keys [pages page-count] :as report}]
  (assoc report
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

(defn render-pdf-page+ [{:keys [pdf-path image-path page type]}]
  (exec+ "/usr/bin/pdftoppm"
         pdf-path
         (string/replace image-path #"\.(png|jpg)$" "")
         (if (= type "jpg")
           "-jpeg"
           (str "-" type))
         "-f"
         page
         "-singlefile"))

(defn file-size+ [path]
  (-> (.stat fs path)
      (.then #(.-size %))))

(defn render-page+ [{:keys [pdf-path]
                     {:keys [page] :as interesting-page} :interesting-page}]
  (let [renders+ (->> ["png" "jpg"]
                      (map (fn [type]
                             (let [image-path (str pdf-path "-" page "." type)
                                   interesting-page* (merge interesting-page
                                                            {:image-path image-path
                                                             :content-type (if (= type "jpg")
                                                                             "image/jpeg"
                                                                             (str "image/" type))})]
                               (-> (render-pdf-page+ {:pdf-path pdf-path
                                                      :image-path image-path
                                                      :page page
                                                      :type type})
                                   (.then #(file-size+ image-path))
                                   (.then (fn [file-size]
                                            (assoc interesting-page* :image-size file-size))))))))]
    (-> (js/Promise.all renders+)
        (.then (fn [page-images]
                 (->> page-images
                      (sort-by (fn [{:keys [image-size content-type]}]
                                 (if (= content-type "image/jpeg")
                                   (* image-size 1.2)
                                   image-size)))
                      first))))))

(defn add-screenshots+ [{:keys [interesting-pages pdf-path] :as report}]
  (-> (js/Promise.all (map (fn [interesting-page]
                             (render-page+ {:pdf-path pdf-path
                                            :interesting-page interesting-page}))
                           interesting-pages))
      (.then (fn [new-pages]
               (assoc report :interesting-pages new-pages)))))

(defn add-interesting-pages-with-screenshots+ [report]
  (-> (download-pdf+ report)
      (.then add-page-count+)
      (.then add-text-content+)
      (.then select-interesting-pages+)
      (.then add-screenshots+)))
