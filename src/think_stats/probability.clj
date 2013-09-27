(ns think-stats.probability
  (:require (think-stats [util :as util]
                         [homeless :as h]
                         [cdf :as cdf]
                         [random :as random]))
  (:import org.apache.commons.math3.distribution.BinomialDistribution))

(defn event
  "Returns a funtion that evaluates to the specified events with the given probabilities.

  Example: This example creates an event that will return a 0 5% of the time,
  a 2 5% of the time and a 1 90% of the time.
  (def e (event {0 0.05 2 0.05 1 0.9})
  (repeatedly 1000 #(e (rand))"
  [event-prob-map]
  (assert (map? event-prob-map))
  (let [m (into (sorted-map-by (fn [k1 k2]
                                 (compare [(get event-prob-map k1) k1]
                                          [(get event-prob-map k2) k2])))
                event-prob-map)
        ks (reductions + (vals m))
        vs (keys m)]
    (fn [p]
      (cdf/cdf->value vs ks p))))

(defn bernoulli-event
  "Generates a function than when called generates a 1 with probability p and a 0 with probability
  1 - p."
  [p]
  (fn []
    (if (<= (rand 1) p)
      1
      0)))


(defn independent-and
  "P(A and B) = P(A) P(B)"
  [a b]
  (* a b))

(defn dependent-and
  "P(A and B) = P(B) P(A|B). The chance that the first thing happens and then 
  the second given the first. This evaluates to the probability of B."
  [a b]
  (/ (* a b)
     b))

(defn p-and
  [a b]
  (* b (dependent-and a b)))

(defn stress-strength-prob
  "Compute the probability that random variable A is less than random variable B (P(A < B)).
  References:
  http://stats.stackexchange.com/questions/24693/probability-that-random-variable-b-is-greater-than-random-variable-a
  http://en.wikipedia.org/wiki/Stress%E2%80%93strength_analysis"
  [a-mean a-var b-mean b-var]
  (/ (- b-mean a-mean)
     (Math/sqrt
       (+ a-var b-var))))


(defn n-choose-k
  "Compute the binomial coefficient \"n choose k\"."
  [n k]
  (cond
    (= k 0) 1
    (< k 0) 0
    (< n 0) 0
    (> k n) 0
    :else
    (/ (util/factorial n)
       (* (util/factorial k)
         (util/factorial
           (- n k))))))

(def binomial-coefficient n-choose-k)

(defn binomial-pmf
  "PMF(k) of the binomial distribution

  (* (n-choose-k n k)
    (Math/pow p k)
    (Math/pow (- 1 p) (- n k)))

  org.apache.commons.math3.distribution.BinomialDistribution is faster.
  "
  [n k p]
  (.probability (random/binomial n p) k))

(defn bayes
  [p-hypothesis p-evidence-give-hypothesis p-evidence]
  (/ (* p-hypothesis p-evidence-give-hypothesis) p-evidence))

(defn p-drug-use-given-positive
  [p-hypothesis specificity sensitivity]
  (bayes p-hypothesis sensitivity
         (+ (* p-hypothesis sensitivity)
            (* (h/numeric-complement p-hypothesis)
               (h/numeric-complement specificity)))))
