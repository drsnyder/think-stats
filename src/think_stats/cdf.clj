(ns think-stats.cdf
  (:require (think-stats 
             [homeless :as h]))
  (:import [think-stats.types]))

(declare cdf cdf->value cdf->probability)

(defmulti cdff class)

(defmethod cdff ::seq [s &{:keys [to-float] :or {to-float false}}]
  (let [m (cdf s)
        kys (keys m)
        vls (vals m)
        vls (if to-float (map float vls) vls)]
    (fn [x &[direction]]
      (if (and (not (nil? direction)) (= direction :value))
        (cdf->value kys vls x)
        (cdf->probability kys vls x)))))

(defn cdf
  [s &{:keys [to-float] :or {to-float true}}]
  (assert (sequential? s) "Cannot compute the cdf on a non-seq data set.")
  (let [s (sort s)
        len (count s)]
    (into (sorted-map)
          (for [[r idx] (map vector s (range 1 (inc len)))
                :let [y (/ idx len)
                      y (if to-float (float y) y)]]
            [r y]))))


(defn cdf->probability
  [kys vls x]
  (cond
    (< x (first kys)) 0
    (> x (last kys))  1
    :else 
    (let [kidx (h/bisect kys x :left)]
      (nth vls kidx))))

(defn cdf->value
  [kys vls prob]
  (cond
    (< prob 0) nil
    (> prob 1) nil
    :else
    (let [vidx (h/bisect vls prob :left)]
      (nth kys vidx))))

