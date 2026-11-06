(ns bot.source-sections
  (:require [clojure.string :as str]))

(defn section [lines title-pattern fallback-title]
  (if-let [start (first (keep-indexed (fn [idx {:keys [text]}]
                                        (when (re-matches title-pattern text) idx)) lines))]
    (let [{heading :text} (nth lines start)
          [_ number title] (re-matches #"^\s*(\d+(?:\.\d+)*)\s+(.+?)\s*$" heading)
          depth (count (str/split number #"\."))
          content (->> (drop (inc start) lines)
                       (take-while (fn [{:keys [text]}]
                                     (let [[_ next-number] (re-matches #"\s*(\d+(?:\.\d+)+)\s+[A-Za-zÄÖÜäöüß].*" text)]
                                       (not (and next-number
                                                 (<= (count (str/split next-number #"\.")) depth))))))
                       vec)]
      {:title title :pages (vec (distinct (cons (:page (nth lines start)) (map :page (remove #(str/blank? (:text %)) content)))))
       :text (str/trim (str/join "\n" (map :text content)))})
    {:title fallback-title :pages [] :text nil}))

(defn extract [report]
  ;; Keep original extraction, including figures/participant lists within a section.
  ;; Require a numbered heading on one line to avoid matching table-of-contents entries.
  (let [lines (vec (mapcat (fn [{:keys [page text]}]
                             (map #(hash-map :page page :text %)
                                  (str/split-lines text))) (:pages report)))]
    [(section lines #"\s*\d+(?:\.\d+)*\s+(?:Zusammenfassung des Ereignisses|Kurzbeschreibung des Ereignisses)\s*"
              "Zusammenfassung des Ereignisses / Kurzbeschreibung des Ereignisses")
     (section lines #"\s*\d+(?:\.\d+)*\s+Folgen\s*" "Folgen")]))

(defn expand-ligatures [text]
  ;; Expand only standard Latin ligatures; preserve symbols such as m² and ①.
  (str/replace text #"[ﬀﬁﬂﬃﬄﬅﬆ]" {"ﬀ" "ff" "ﬁ" "fi" "ﬂ" "fl" "ﬃ" "ffi" "ﬄ" "ffl" "ﬅ" "st" "ﬆ" "st"}))

(defn join-detached-bullets [text]
  ;; Poppler often emits the list marker, a blank line, then the item text.
  ;; Keep the marker's indentation; leave orphan markers/section headings alone.
  (str/replace text
               #"(?m)^([ \t]*[•◦▪‣●])[ \t]*\r?\n(?:[ \t]*\r?\n)*[ \t]*(?!\d+(?:\.\d+)*[ \t]|Seite[ \t]+\d)(?=[^\s•◦▪‣●])"
               "$1 "))

(defn page-text [text]
  (-> text expand-ligatures join-detached-bullets))

(defn prose [text]
  (let [unwrapped (-> (or text "") page-text
                      (str/replace #"\u00ad" "")
                      (str/replace #"([A-Za-zÄÖÜäöüß])[-‐][ \t]*\n\s*(?!(?:und|oder|sowie|bzw)\b)([a-zäöüß])" "$1$2"))
        blocks (->> (str/split unwrapped #"(?m)\n[ \t]*\n|(?=^[ \t]*[•◦▪‣●][ \t]+)")
                    (map #(-> % (str/replace #"\s+" " ") str/trim))
                    (remove str/blank?))]
    (reduce (fn [text block]
              (str text (when-not (empty? text)
                          (if (re-find #"^[•◦▪‣●][ \t]" block) "\n" "\n\n")) block))
            "" blocks)))

(defn clean-page [report {:keys [text] :as page}]
  (assoc page :text
         (->> (str/split-lines (str/replace text #"(?m)^\s*\d+\s*\n\s*(?=Quelle:)" ""))
              (map-indexed (fn [idx line]
                             (let [trimmed (str/trim line)]
                               (when-not (or (= trimmed (:report-type report))
                                             (and (< idx 4) (re-find #"^.+,\s*\d{2}\.\d{2}\.\d{4}," trimmed))
                                             (re-matches #"Seite\s+\d+\s+von\s+\d+" trimmed)
                                             (re-find #"^Abbildung\s+\d+[: .]" trimmed)
                                             (re-find #"^(?:\d+\s+)?Quelle:" trimmed))
                                 line))))
              (remove nil?) (str/join "\n"))))

(defn extract-clean [report]
  (-> report
      (update :pages #(mapv (partial clean-page report) %))
      extract))
