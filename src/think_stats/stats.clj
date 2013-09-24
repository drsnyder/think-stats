(ns think-stats.stats
  (:require (think-stats
              [util :as util]
              [types :as types]
              [distributions :as d]
              [cdf :as cdf]
              [homeless :as h]))
  (:import org.apache.commons.math3.distribution.TDistribution))


(defn mean
  ([s n]
   (assert (sequential? s) "Cannot compute the mean on a non-seq.")
   (if (empty? s)
     nil
     (/ (h/sum s)
        n)))
  ([s]
   (mean s (count s))))

(defn percentile-median
  [s &{:keys [sorted] :or {sorted false}}]
  (d/percentile-w s 50 :sorted sorted))

(defn median
  [s &{:keys [sorted] :or {sorted false}}]
  (let [n (count s)
        s (if (not sorted) (sort s) s)]
    (assert (> n 0))
    (cond
      (even? n) (let [upper-idx (/ n 2)
                      lower-idx (dec upper-idx)
                      {upper upper-idx lower lower-idx} (vec s)]
                  (/ (+ upper lower) 2))
      :else (nth s (quot n 2)))))


(defmulti mean-variance
  "Compute the mean difference of each element in s and apply f to computed value.
  The seq s can be either a map or a sequence. In the case of a map, the seq is treated
  as a histogram."
  (fn [s f n m] (class s)))

(defmethod mean-variance :types/seq
  [s f n m]
  (/ (h/sum (map #(f (- % m)) s))
     n))

(defmethod mean-variance :types/map
  [s f n m]
  ; when we have a map we are working with a histogram. in this case, multiply
  ; each mean difference (key) by the weight (value)
  (/ (h/sum (map #(* (f (- (first %) m)) (second %)) (seq s)))
     n))

(defn variance
  ([s total sample-size]
   (mean-variance s h/square sample-size (mean s total)))
  ([s sample-size]
   (variance s (count s) sample-size))
  ([s]
   (variance s (count s))))

(defn mean-cubed-variance
  [s]
  ((mean-variance s h/cube (count s))))

(defn stddev
  ([s n]
   (Math/sqrt (variance s n)))
  ([s]
   (stddev s (count s))))

(defn mN
  "Generalized fn for computing m2 and m3."
  ([f s total mean]
   (mean-variance s f total mean))
  ([f s total]
   (mN f s total (mean s total)))
  ([f s]
   (mN f s (count s))))

(def m2 (partial mN h/square))
(def m3 (partial mN h/cube))

(defn g1
  "Compute the g1 skewness coefficient given m2 and m3."
  [m2 m3]
  (/ m3 (Math/pow m2 3/2)))

(defn seq->g1
  "Given a sequence, compute g1."
  [s]
  (assert (sequential? s) "Cannot compute g1 skewness coefficient on a non-seq.")
  (g1 (m3 s) (m2 s)))

(def skew g1)

(defn gp
  "Compute Pearson's median skewness coefficient given the mean, median (md) and the standard deviation (std)."
  [mean md std]
  (/ (* 3 (- mean md))
     std))

(defn seq->gp
  "Compute Pearson’s median skewness coefficient from a sequence."
  [s]
  (assert (sequential? s) "Cannot compute Pearson's median skewness coefficient on a non-seq.")
  (let [mean (mean s)
        md (median s)
        std (stddev s)]
    (gp mean md std)))

(defmulti mean-difference
  "Compute the mean difference given a seq or a PMF. If a map is provided then
  a PMF is assumed."
  (fn [s & args]
    (class s)))

(defmethod mean-difference :types/map
  [s & args]
  (reduce +
          (for [[xi xp :as x] (seq s)
                [yi yp :as y] (seq s) :when (not= x y)]
            (* xp yp (Math/abs (- xi yi))))))

(defmethod mean-difference :types/seq
  ([s n]
   (/
    (reduce +
            (for [x s
                  y s :when (not= x y)]
              (Math/abs (- x y))))
    (h/square n)))
  ([s]
   (mean-difference s (count s))))

(declare trim)

(defn summary
  [s &{:keys [trim? p] :or {trim? false p 0.01}}]
  (let [s (if trim? (trim s p) (sort s))
        len (count s)]
    {:min (first s)
     :25th (d/percentile-w s 25 :sorted true)
     :median (median s :sorted true)
     :mean (float (mean s))
     :stddev (stddev s (- len 1))
     :75th (d/percentile-w s 75 :sorted true)
     :95th (d/percentile-w s 95 :sorted true)
     :max (last s)
     :count (count s)}))

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

(defn create-t-dist
  "Create a t-distribution object. Uses org.apache.commons.math3.distribution.TDistribution."
  [dof]
  (TDistribution. dof))

(def t-distribution (memoize create-t-dist))


(defn z
  "Compute a z score given a raw score, mean, and standard or sample deviation.
  raw: raw score
  mean: sample mean
  stddev: sample standard deviation"
  [raw mean stddev]
  (/ (- raw mean)
     stddev))

(defn t
  "Compute the t value given two sample means and a standard error."
  [raw mean standard-error]
  (/ (- raw mean)
     standard-error))

(defn z->area
  "Given a z score, compute the cumulative area under the normal distribution.
  The standard normal distribution has μ = 0 (mu) and σ = 1 (sigma)."
  [z-score]
  (let [mu 0 sigma 1]
    (cdf/normalcdf mu sigma z-score)))

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
  (let [a (.density (t-distribution dof) t-value)]
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
    (Math/abs (.inverseCumulativeProbability (t-distribution dof) p))))


(comment
  (def sample  (repeatedly 1000 random/rankit-sample))
  (stats/mean-squared-error 0  (map stats/mean sample) 1000))

(defn mean-error
  "Compute the mean error given an exstimator and a collection."
  ([estimator mean-coll m f]
   (/ (h/sum (map #(f (- % estimator)) mean-coll)) m))
  ([estimator mean-coll m]
   (mean-error estimator mean-coll m identity))
  ([estimator mean-coll]
   (mean-error estimator mean-coll (count mean-coll))))


(defn mean-squared-error
  "Compute the mean squared error given an estimator (typically the population mean)."
  ([estimator mean-coll m]
   (mean-error estimator mean-coll m h/square))
  ([estimator mean-coll]
   (mean-squared-error estimator mean-coll (count mean-coll) h/square)))
