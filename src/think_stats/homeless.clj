(ns think-stats.homeless
  (:require (think-stats [util :as util])))

(defn sum
  [s]
  (reduce + s))


(defn filter-map
  "Filter a map by the keys matching (f k)."
  [m f]
  (select-keys m (for [[k v] m
                       :when (f k)]
                   k)))

(defn map-map
  [f m &{:keys [dest] :or {dest {}}}]
  (into dest (for [[k v] m] [k (f v)])))

(defn map->into
  [m dest]
  (into dest (for [[k v] m] [k v])))

(defn- vswap!
  [v a b]
  (let [t (nth v b)]
    (assoc! v b (nth v a))
    (assoc! v a t)))

(defn- partition-by-idx [l left right idx]
  (let [pivot-value (nth l idx)
        store-index (atom left)]
    (vswap! l idx right)
    (loop [i left]
      (when (< (nth l i) pivot-value)
        (vswap! l @store-index i)
        (swap! store-index inc))
      (when (< i right)
        (recur (inc i))))
    (vswap! l right @store-index) 
    @store-index))

(defn- select-with-transient
  [l left right k]
    (loop [lt (transient l) left left right right k k]
      (if (= left right)
        (nth lt left)
        ; this is an arbitrary selection of the pivot index that may not serve use
        ; well in some cases
        (let [new-index (partition-by-idx lt left right (quot (+ left right) 2))
              dist (inc (- new-index left))
              found (if (= dist k) (nth lt new-index))
              [new-left new-right new-k] (if 
                                           (< k dist) [left (dec new-index) k]
                                           [(inc new-index) right (- k dist)])]
              (if found
                found
                (recur lt new-left new-right new-k))))))

(defn select
  "See http://en.wikipedia.org/wiki/Selection_algorithm"
  [l left right k]
  (select-with-transient l left right k))


(defmulti compute-bisection-ends (fn [dir s x low mid high] dir))

(defmethod compute-bisection-ends :right 
  ([dir s x low mid high]
   (cond
     (< x (nth s mid)) [low mid]
     :else [(inc mid) high])))

(defmethod compute-bisection-ends :left
  ([dir s x low mid high]
   (cond
     (< (nth s mid) x) [(inc mid) high]
     :else [low mid])))
  
(defmethod compute-bisection-ends :default
  ([dir s x low mid high]
   (compute-bisection-ends :right s x low mid high)))


(defn bisect
  "When dir is :right find the insertion point idx in s such that all e (take idx x) satisfies e <= x.
  When dir il :left find the insertion point idx in s such that all e (take idx x) satisfies e < x.

  Assumes that s is sorted."
  [s x & [dir]]
  (assert (sequential? s) "Cannot bisect a non-seq.")
  (loop [low 0
         high (dec (count s))]
    (if (>= low high)
      low
      (let [mid (quot (+ low high) 2)
            [l h] (compute-bisection-ends dir s x low mid high)]
        (recur l h)))))

(defn approximately-equal
  ([a b r]
   (>= r (Math/abs (- a b))))
  ([a b]
   (approximately-equal a b 0.01)))

(defn round
  "Round number to decimals digits."
  ([number decimals]
   (let [factor (Math/pow 10 decimals)]
     (bigdec (/ (Math/round (* factor number)) factor))))
  ([number]
   (round number 0)))

(defn square [x] (* x x))

(defn cube [x] (* x x x))

(defn numeric-complement
  "Numeric complement. 1 - n."
  [n]
  (assert (number? n) (str "Error numeric-complement: " (type n) " is not a number."))
  (- 1 n))

(defn dissoc-vec
  "dissoc index i from vector v."
  [v i]
  (let [c (count v)]
    (assert (vector? v) "Can only dissoc-vec on a vector.")
    (assert (and (>= i 0) (< i c)) "Vector index out of bounds.")
    (cond
      (= i 0) (subvec v 1)
      (= i (- c 1)) (subvec v 0 i)
      :else (into (subvec v 0 i) (subvec v (inc i))))))
