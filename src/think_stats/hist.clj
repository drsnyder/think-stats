(ns think-stats.hist
  (:require (think-stats [util :as util]
                         [types :as types]
                         [homeless :as h])))

(defmulti hist
  "Build a histogram from a sequence or a map. The resulting
  histogram will be key sorted unless an alternate destination is supplied."
  (fn [i &[dest]] (class i)))

(defmethod hist :types/seq
  [s &{:keys [dest] :or {dest (sorted-map)}}]
  (h/map->into (frequencies s) dest))

(defmethod hist :types/map
  [m &{:keys [dest] :or {dest (sorted-map)}}]
  (prn m)
  (h/map->into m dest))


(declare hist->mass)
(declare hist->area)

(defn hist->pmf
  "Convert a histogram into a PMF."
  [h &{:keys [to-float] :or {to-float false}}]
  (assert (map? h) "Cannot compute the PMF of a histogram on a non-map.")
  (let [total (hist->mass h)]
    (into (sorted-map) (for [[k v] (seq h)
                             :let [m (/ v total)]]
                         [k m]))))

(defn pmf
  "Generate a PMF from a seq."
  [s &{:keys [to-float] :or {to-float false}}]
  (assert (sequential? s) "Cannot compute the pmf on a non-seq.")
  (hist->pmf (hist s)))

(defn hist->cdf
  "Compute the CDF of a histogram."
  [h]
  (assert (map? h) "Cannot compute the CDF of a histogram on a non-map.")
  (let [total (h/sum (vals h))]
    (into (sorted-map) (for [[i v] (map vector (keys h) (reductions + (vals h)))]
                         [i (/ v total)]))))

(defn hist->mean
  "Compute the mean given a histogram."
  [h]
  (assert (map? h) "Cannot compute the mean of a histogram on a non-map.")
  (let [mass (hist->mass h)]
    (/ (hist->area h)
       mass)))

(defn hist->mass
  [h]
  (assert (map? h) "Cannot compute the total mass of a histogram on a non-map.")
  (reduce + (vals h)))

(defn hist->bin-widths
  "Compute the bin widths of the histogram."
  [h]
  (assert (map? h) "Cannot compute the bin widths a histogram on a non-map.")
  (map (partial apply -) (map reverse (partition 2 1 (keys h)))))

(defn hist->area
  "Compute the area of a histogram. Assumes the items represent with bin widths."
  [h]
  (assert (map? h) "Cannot compute the total area of a histogram on a non-map.")
  (assert (empty? (filter (complement number?) (keys h))) "Cannot compute the using non-numerical items.")
  (reduce + (map (partial apply *) (seq h))))



(defn pmf-entry->value
  [e]
  (first e))

(defn pmf-entry->freq
  [e]
  (second e))


(defn pmf->key-ordered
  [pmf &{:keys [dir] :or {dir :asc}}]
  (apply sorted-map-by (if (= dir :asc) < >) (flatten (seq pmf))))


(defn pmf->remaining-lifetime
  [pmf]
  (loop [lifetimes (sort > (keys pmf))
         acc {}
         total 0]
    (if (not (empty? lifetimes))
      (let [k (first lifetimes)
            new-total (+ (pmf k) total)]
        (recur (rest lifetimes)
               (assoc acc k new-total)
               new-total))
      acc)))

(defn pmf->mean
  [pmf]
  (h/sum
    (map #(* (pmf-entry->value %)
            (pmf-entry->freq %))
         (seq pmf))))

(defn pmf->variance
  [pmf]
  (let [m (pmf->mean pmf)]
    (h/sum
      (map #(* (pmf-entry->freq %)
              (h/square (- (pmf-entry->value %) m)))
           pmf))))

(defn pmf->stddev
  [pmf]
  (Math/sqrt (pmf->variance pmf)))


(defn bin-pmf-freq
  "Bin PMF values by applying binfn to each value in the PMF.
  The resulting bins contain the sum of the frequencies that mapped to that bin."
  [pmf binfn]
  (h/sum (map pmf-entry->freq
            (filter #(binfn (pmf-entry->value %))
                    (seq pmf)))))

(defn normalize-pmf
  ([pmf fraction]
   (if (empty? pmf)
     pmf
     (let [factor (/ fraction (h/sum (vals pmf)))]
       (into {} (for [[k v] pmf] [k (* v factor)])))))
  ([pmf]
   (normalize-pmf pmf 1.0)))


(defn pmf->biased
  "Bias a PMF (or unbias if invert is true). If the PMF is the distribution of reported values then
  any oversampling would be in proportion to the values. See the class size example. And the 3-size plot."
  [pmf &{:keys [invert] :or {invert false}}]
  (let [transf (if invert
                (fn [k v] (* v (/ 1 k))) 
                (fn [k v] (* v k)))
        t (into {} (for [[k v] pmf] [k (transf k v)]))]
    (normalize-pmf t)))

(defn pmf->unbiased
  [pmf]
  (pmf->biased pmf :invert true))


(defn pmf+->pmf
  "Given two PMFs x and y generate a new PMF z that is the sum of x and y."
  [x y]
  (assert (and (map? x)
               (map? y)) "Both x and y must be maps.")
  (hist->pmf (hist (for [xv (keys x)
                         yv (keys y)]
                     (+ xv yv)))))

(defn pmf-max->pmf
  "Given two PMFs x and y generate a new PMF z that is the max(x, y)."
  [x y]
  (assert (and (map? x)
               (map? y)) "Both x and y must be maps.")
  (hist->pmf (hist (for [xv (keys x) yv (keys y)]
                     (max xv yv)))))

(defn uniform-pmf
  [low high steps]
  (pmf (for [i (range steps)]
         (+ low (/ (* i (- high low)) (- steps 1.0))))))
