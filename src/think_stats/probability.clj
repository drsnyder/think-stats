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

