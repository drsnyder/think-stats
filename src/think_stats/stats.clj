(ns think-stats.stats
  (:require (think-stats
              [distributions :as d]
              [homeless :as h])))

(defn sum
  [s]
  (reduce + s))

(defn mean
  [s]
  (assert (sequential? s) "Cannot compute the mean on a non-seq.")
  (if (empty? s)
    nil
    (/ (sum s) 
       (count s))))


(defn variance
  [s &{:keys [sample] :or {sample false}}]
  (assert (sequential? s) "Cannot compute the variance on a non-seq.")
  (let [n (if sample 
            (- (count s) 1) 
            (count s))]
    (when-let [m (mean s)]
      (/ (sum (map #(h/square (- % m)) s))
         n))))

(defn stddev
  [s]
  (Math/sqrt (variance s)))


(defn summary
  [s]
  (let [s (sort s)]
    {:min (first s)
     :25th (d/percentile-w s 25 :sorted true)
     :median (d/percentile-w s 50 :sorted true)
     :mean (mean s)
     :75th (d/percentile-w s 75 :sorted true)
     :95th (d/percentile-w s 95 :sorted true)
     :max (last s)}))


(defn hist
  [s]
  (assert (sequential? s) "Cannot compute the hist on a non-seq.")
  (into (sorted-map) (for [[k v] (frequencies s)] [k v])))


(defn trim
  "Trim's s by taking p % elements from both ends. The sequence is sorted before being trimmed."
  ([s p]
   (assert (sequential? s) "Cannot trim a non-seq.")
   (let [s (sort s)
         len (count s)
         n (int (* p len))]
     (drop n (take (- len n) s))))
  ([s]
   (trim s 0.01)))


(defn pmf
  [s &{:keys [to-float] :or {to-float false}}]
  (assert (sequential? s) "Cannot compute the pmf on a non-seq.")
  (let [n (count s)]
    (into (sorted-map) (for [[k v] (frequencies s)
                   :let [m (/ v n)
                         m (if to-float (float m) m)]]
               [k m]))))

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
  (sum
    (map #(* (pmf-entry->value %) 
            (pmf-entry->freq %)) 
         (seq pmf))))

(defn pmf->variance
  [pmf]
  (let [m (pmf->mean pmf)]
    (sum 
      (map #(* (pmf-entry->freq %)
              (h/square (- (pmf-entry->value %) m)))
           pmf))))


(defn bin-pmf-freq
  "Bin PMF values by applying binfn to each value in the PMF. 
  The resulting bins contain the sum of the frequencies that mapped to that bin."
  [pmf binfn]
  (sum (map pmf-entry->freq 
            (filter #(binfn (pmf-entry->value %)) 
                    (seq pmf)))))

(defn filter-map
  "Filter a map by the keys matching (f k)."
  [pmf f]
  (select-keys pmf (for [[k v] pmf :when (f k)] k)))


(defn normalize-pmf
  ([pmf fraction]
   (if (empty? pmf)
     pmf
     (let [factor (/ fraction (sum (vals pmf)))]
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


(defn z
  "Compute a z score given a raw score, mean, and standard or sample deviation."
  [raw mean stddev]
  (/ (- raw mean)
     stddev))


(defn z->area
  "Given a z score, compute the cumulative area under the normal distribution.
  The standard normal distribution has μ = 0 (mu) and σ = 1 (sigma)."
  [z]
  (let [mu 0 sigma 1]
    (d/normalcdf mu sigma z)))

