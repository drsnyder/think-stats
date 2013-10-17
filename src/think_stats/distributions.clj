(ns think-stats.distributions
  (:refer-clojure :exclude [partition])
  (:require (think-stats
              [constants :as c]
              [util :as util]
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
  [s x &{:keys [sorted] :or {sorted false}}]
  (assert (sequential? s) "Cannot compute the percentile on a non-seq data set.")
  (let [s (if sorted s (sort s))
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

; TODO testing everything below
(defn average-ranks
  [idx n]
  (if (= n 1)
    (list idx)
    (let [mean (/ (reduce + (range (- idx (dec n)) (inc idx))) n) ]
      (repeat n mean))))

(defn lazy-rank-seq
  ([l]
   (let [s (sort l)
         n (count l)
         order (into (sorted-map)
                     (for [[idx i] (map vector s (range 1 (inc n)))]
                       [idx i]))]

     (lazy-rank-seq order l)))
  ([order l]
   (when-let [rank (order (first l))]
     (cons rank (lazy-seq (lazy-rank-seq order (rest l)))))))

(defn rank-seq
  [l]
  (loop [ls (lazy-rank-seq l)
         acc ()]
    (if (empty? ls)
      acc
      (let [i (first ls)
            batch (conj (take-while #(= i %) (rest ls)) i)
            batch-len (count batch)]
        (recur (drop (count batch) ls)
               (concat acc (average-ranks i batch-len)))))))
