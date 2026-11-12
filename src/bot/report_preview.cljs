(ns bot.report-preview
  (:require ["fs/promises" :as fs]
            [bot.http :as http]
            [bot.pdf :as pdf]
            [bot.report :as bot]
            [clojure.string :as str]))

(defn json [value]
  (.stringify js/JSON (clj->js value) nil 2))

(defn write-json+ [path value]
  (fs/writeFile path (json value)))

(defn read-json-if-present+ [path]
  (-> (fs/readFile path "utf8")
      (.then #(js->clj (.parse js/JSON %) :keywordize-keys true))
      (.catch (fn [error]
                (if (= "ENOENT" (.-code error)) nil (throw error))))))

(defn prepare-pdf+ [report directory]
  (-> (fs/mkdir directory #js {:recursive true})
      (.then (fn []
               (if (:pdf-path report)
                 (-> (fs/copyFile (:pdf-path report) (str directory "/report.pdf"))
                     (.then #(assoc report :pdf-path (str directory "/report.pdf"))))
                 (let [path (str directory "/report.pdf")]
                   (-> (http/raw-request+ (:report-pdf-uri report) {:method "GET"})
                       (.then (fn [response]
                                (when-not (.-ok response)
                                  (throw (ex-info "Could not download report PDF" {})))
                                (.arrayBuffer response)))
                       (.then #(fs/writeFile path (js/Buffer.from %)))
                       (.then #(assoc report :pdf-path path)))))))
      (.then pdf/add-page-count+)
      (.then pdf/add-text-content+)
      (.then bot/add-post)))

(defn selected-pages [report selections]
  ;; Only the visual description comes from the model; retain every character of extracted text.
  ;; Preserve its order, including any explicitly justified departure from PDF page order.
  (mapv (fn [selection]
          (let [page (nth (:pages report) (dec (:page selection)))
                description (if (string? (:image-description selection))
                              (str/trim (:image-description selection)) "")
                alt (if (str/blank? description)
                      (:text page)
                      (str "Bildbeschreibung:\n" description "\n\nText der Seite:\n" (:text page)))]
            (merge page (select-keys selection [:page :reason :roles :focus])
                   {:image-description description :alt alt})))
        selections))
