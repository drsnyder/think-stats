(ns think-stats.distributions
  (:refer-clojure :exclude [partition])
  (:require (think-stats
              [util :as util]
              [stats :as stats]
              [homeless :as h])))


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
  (assert (sequential? s) "Cannot compute the cdf on a non-seq.")
  (let [s (sort s)
        len (count s)
        c (+ (* (/ x 100) len) 0.5)
        idx (dec c)] ; zero offset
    (nth s idx)))


(defn compute-cdf
  [s x]
  (assert (sequential? s) "Cannot compute the cdf on a non-seq.")
  (/ 
    (count (filter #(<= % x) s)) 
    (count s)))

(defn cdf
  [s]
  (assert (sequential? s) "Cannot compute the cdf on a non-seq.")
  (let [len (count s)]
    (into {} 
          (loop [s' s idx 1 acc []]
            (if (empty? s')
              acc
              (recur (rest s') (inc idx) (conj acc [(first s') (/ idx len)])))))))



