(ns think-stats.distributions
  (:refer-clojure :exclude [partition])
  (:require (think-stats
              [util :as util]
              [homeless :as h]))
  (:import org.apache.commons.math3.special.Erf))

(def ^:private sqrt2 (Math/sqrt 2))

(defn percentile
  "A more efficient percentile that uses a selection algorithm
  http://en.wikipedia.org/wiki/Selection_algorithm. 

  \"More efficient\" should be taken with a grain of salt. YMMV and it probably depends on the data set.
  "
  [s k]
  (let [scaled-k (* (count s) (/ k 100))
        s (vec s)] 
    (h/select s 0 (dec (count s)) scaled-k)))

(defn percentile-rank
  [scores yours]
  (* (/ 
       (count (filter #(<= % yours) scores)) 
       (count scores)) 
    100.0))

(defn percentile-s
  "For illustration purposes."
  [scores rank]
  (let [scores (sort scores)
        len (count scores)
        rank (/ rank 100)]
    (loop [s scores
           current-rank 1]
      (if (>= (/ current-rank len) rank)
        (first s)
        (recur (rest s) (inc current-rank))))))

(defn percentile-w
  "Wikipedia implementation http://en.wikipedia.org/wiki/Percentile"
  [s x]
  (assert (sequential? s) "Cannot compute the percentile on a non-seq data set.")
  (let [s (sort s)
        len (count s)
        x (max 0 (min x 100))
        c (+ (* (/ x 100) len) 0.5)
        idx (dec c)] ; zero offset
    (nth s idx)))


(defn compute-cdf-value
  [s x]
  (assert (sequential? s) "Cannot compute the cdf on a non-seq data set.")
  (/ 
    (count (filter #(<= % x) s)) 
    (count s)))

(defn cdf
  [s &{:keys [to-float] :or {to-float false}}]
  (assert (sequential? s) "Cannot compute the cdf on a non-seq data set.")
  (let [s (sort s)
        len (count s)]
    (into (sorted-map)
          (for [[r idx] (map vector s (range 1 (inc len)))
                :let [y (/ idx len)
                      y (if to-float (float y) y)]]
            [r y]))))

(defn- cdf->probability
  [kys vls x]
  (cond
    (< x (first kys)) 0
    (> x (last kys))  1
    :else 
    (let [kidx (h/bisect kys x :left)]
      (nth vls kidx))))

(defn- cdf->value
  [kys vls prob]
  (cond
    (< prob 0) nil
    (> prob 1) nil
    :else 
    (let [vidx (h/bisect vls prob :left)]
      (nth kys vidx))))


; should this be a protocol and a type?
(defn cdff
  "Returns a function (f x) that computes the CDF(x) = p and it's inverse from the data set s.

  (def cdf (cdff (range 1 101)))
  (cdf 10) => 0.1
  (cdf 10 :probability) => 0.1
  (cdf 0.1 :value) => 10

  "
  [s &{:keys [to-float] :or {to-float false}}]
  (assert (sequential? s) "Cannot compute the cdf on a non-seq data set.")
  (let [m (cdf s)
        kys (keys m)
        vls (vals m)
        vls (if to-float (map float vls) vls)]
    (fn [x &[direction]]
      (if (and (not (nil? direction)) (= direction :value))
        (cdf->value kys vls x)
        (cdf->probability kys vls x)))))


(defn cdf->median
  "Given a cdf fn generated by cdff, compute the median."
  [cdf]
  (cdf 0.5 :value))

(defn cdf->interquartile
  "Given a cdf fn generated by cdff, compute the interquartile range. Returns a vec with the 25th percentile,
  the mean and the 75th percentile in that order."
  [cdf]
  [(cdf 0.25 :value) (cdf 0.5 :value) (cdf 0.75 :value)])


(defn sample
  "Generate a lazy seq of values chosen at random from the given cdf. See cdff above.

  (def cdf (cdff (take 50 (repeatedly #(rand-int 10)))))
  (sample cdf 10)
  "
  [cdf n]
  (for [i (range n)]
    (cdf (rand) :value)))


; TODO: testing. how?
(defn expovariate
  "See http://en.wikipedia.org/wiki/Exponential_distribution generating exponential variates"
  [lambda]
  (* -1.0 (/ (Math/log (- 1.0 (rand))) (float lambda))))

(defn expomedian
  [lambda]
  (/ (Math/log 2) lambda))

(defn expomean
  [lambda]
  (/ 1.0 lambda))



(defn paretovariate
  "See http://en.wikipedia.org/wiki/Pareto_distribution for random sample generation."
  ([alpha x-min]
   (* x-min 
     (/ 1.0 (Math/pow (- 1.0 (rand)) 
                      (/ 1 alpha)))))
  ([alpha] 
   (paretovariate alpha 1)))
      
(defn paretomedian
  "Compute the median of a Pareto distribution with the given alpha and threshold."
  ([alpha x-min]
   (* x-min (Math/pow 2 (/ 1 alpha))))
  ([alpha]
   (paretomedian alpha 1)))

(defn paretomean
  "Compute the mean of a Pareto distribution with the given alpha and threshold."
  ([alpha x-min]
   (assert (> alpha 1))
   (/ (* alpha x-min)
      (- alpha 1)))
  ([alpha]
   (paretomean alpha 1)))


(defn weibullvariate
  "See http://en.wikipedia.org/wiki/Weibull_distribution.

  A transformation that should produce a straight line:
  log(-log(y)) = log(x)
  [ log(-log(y)) = klog(x) - klog(lambda) ]

  Example:
  (def w (repeatedly 100000 #(d/weibullvariate 1 0.5)))
  (def cdf (d/cdf w :to-float true))
  ; < 2.5 to match the wikipedia page for comparison
  (def cdf (into {} (for [k (filter #(< % 2.5) (keys cdf))] [k (get cdf k))))
  (plots/line (keys cdf) (vals cdf))
  "
  [lambda beta]
  (* lambda (Math/pow (* -1.0 (Math/log (- 1.0 (rand))))
                      (/ 1.0 beta))))


(defn normalvariate
  "Generate random values from a normal distribution with mean mu and standard deviation sigma."
  [mu sigma]
  (let [p (rand)
        x (* sqrt2 (Erf/erfInv (- (* 2 p) 1)))]
    (+ (* sigma x) mu)))
