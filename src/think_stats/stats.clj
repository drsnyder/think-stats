(ns think-stats.stats
  (:require (think-stats
              [distributions :as d]
              [homeless :as h]
              [random :as random])))

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
  [s &{:keys [sample] :or {sample false}}]
  (Math/sqrt (variance s :sample sample)))


(declare trim)

(defn summary
  [s &{:keys [trim? p] :or {trim? false p 0.01}}]
  (let [s (if trim? (trim s p) (sort s))]
    {:min (first s)
     :25th (d/percentile-w s 25 :sorted true)
     :median (d/percentile-w s 50 :sorted true)
     :mean (float (mean s))
     :stddev (stddev s :sample true)
     :75th (d/percentile-w s 75 :sorted true)
     :95th (d/percentile-w s 95 :sorted true)
     :max (last s)
     :count (count s)}))


(defn hist
  [s]
  (assert (sequential? s) "Cannot compute the hist on a non-seq.")
  (into (sorted-map) (for [[k v] (frequencies s)] [k v])))


(defn trim
  "Trim's s by taking p % elements from both ends. The sequence is sorted before being trimmed.
  The params :right and :left can be toggled with booleans to selectively trim either side of the data set.

  (trim data 0.02) ; => trims 2% from both sides
  (trim data 0.02 :left false) ; => only trims 2% from the right
  "
  ([s p &{:keys [right left] :or {right true left true}}]
   (assert (sequential? s) "Cannot trim a non-seq.")
   (let [s (sort s)
         len (count s)
         n (int (* p len))]
     (cond->> s
              right (take (- len n))
              left  (drop n))))
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
  "Compute a z score given a raw score, mean, and standard or sample deviation.
  raw: raw score
  mean: sample mean
  stddev: sample standard deviation"
  [raw mean stddev]
  (/ (- raw mean)
     stddev))


(defn z->area
  "Given a z score, compute the cumulative area under the normal distribution.
  The standard normal distribution has μ = 0 (mu) and σ = 1 (sigma)."
  [z-score]
  (let [mu 0 sigma 1]
    (d/normalcdf mu sigma z-score)))

(defn p-value
  "Compute the p value for a give z score. The p value is the probability of
  getting a given statistic by chance."
  [z-score]
  (- 1 (z->area (Math/abs z-score))))

(def z->p-value p-value)

(defn standard-error
  "Compute the standard error of a sample statistic."
  [stddev sample-size]
  (/ stddev (Math/sqrt sample-size)))


(defn t->p-value
  "Compute the p-value for a given degrees of freedom and a t-value. This should be equivalent
  to a table of critical values in the t distributions.

  .density gives the PDF(x) for the t distribution.

  References:
    http://www.wolframalpha.com/input/?i=pdf[+studenttdistribution[29]%2C+0.015+]
    http://commons.apache.org/proper/commons-math/apidocs/org/apache/commons/math3/distribution/TDistribution.html"
  [dof t-value &{:keys [one-sided] :or {one-sided false}}]
  (let [a (.density (random/t-distribution dof) t-value)]
    (if one-sided
      (/ a 2)
      a)))


(defn alpha->t
  "Compute the t value for a given alpha level degrees of freedom.
   Example:
   Compute the t value for for a two-tailed test with 120 degrees of freedom and an alpha level of 0.05:

   (alpha->t 120 0.05)

   For a one-tailed test use:

   (alpha->t 120 0.05 :two-tailed false)"
  [dof alpha &{:keys [two-tailed] :or {two-tailed true}}]
  (let [p (if two-tailed (/ alpha 2.0) alpha)]
    (Math/abs (.inverseCumulativeProbability (random/t-distribution dof) p))))
