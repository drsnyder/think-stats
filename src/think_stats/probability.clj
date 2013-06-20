(ns think-stats.probability
  (:require (think-stats [util :as util])))

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
  the second given the first."
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
  "PMF(k) of the binomial distribution."
  [n k p]
  (* (n-choose-k n k)
    (Math/pow p k)
    (Math/pow (- 1 p) (- n k))))
