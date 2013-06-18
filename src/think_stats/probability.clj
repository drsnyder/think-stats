(ns think-stats.probability)

(defn bernoulli-event
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
