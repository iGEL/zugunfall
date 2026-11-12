(ns bot.image-focus)

;; Full-page coordinates, origin at the top left, independent of render size.
(defn valid? [point]
  (and (map? point)
       (every? #(and (number? %) (js/Number.isFinite %) (<= 0 % 1))
               [(:x point) (:y point)])))

(defn mastodon [point]
  (when-not (valid? point)
    (throw (ex-info "Invalid image focus: expected full-page x/y between 0 and 1." {})))
  (str (- (* 2 (:x point)) 1) "," (- 1 (* 2 (:y point)))))
