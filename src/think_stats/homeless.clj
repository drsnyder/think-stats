(ns think-stats.homeless
  (:require (think-stats [util :as util])))


(defn map-map
  [f m &{:keys [dest] :or {dest {}}}]
  (into dest (for [[k v] m] [k (f v)])))

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
         high (count s)]
    (if (>= low high)
      low
      (let [mid (quot (+ low high) 2)
            [l h] (compute-bisection-ends dir s x low mid high)]
        (recur l h)))))
    

(defn approxiately-equal
  ([a b r]
   (>= r (Math/abs (- a b))))
  ([a b]
   (approxiately-equal a b 0.01)))
    
        

