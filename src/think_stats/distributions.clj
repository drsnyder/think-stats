(ns think-stats.distributions
  (:refer-clojure :exclude [partition])
  (:require (think-stats
              [util :as util]
              [stats :as stats]
              [homeless :as h])))

(defn- vswap! 
  [v a b]
  (let [t (nth v b)]
    (assoc! v b (nth v a))
    (assoc! v a t)))

(defn- partition [l left right idx]
  (let [pivot-value (nth l idx)
        store-index (atom left)]
    (vswap! l idx right)
    (doall
      (for [i (range left right)]
        (do
          (when (< (nth l i) pivot-value)
            (vswap! l @store-index i)
            (swap! store-index inc)))))
    (vswap! l right @store-index) 
    @store-index))

; see http://en.wikipedia.org/wiki/Selection_algorithm
(defn- select 
  [l left right k]
  (if (= left right)
    (nth l left)
    (let [new-index (partition l left right (int (/ (+ left right) 2)))
          dist (inc (- new-index left))]
      (cond 
        (= dist k) (nth l new-index)
        (= new-index k) (nth l new-index) ; same as above
        (< k dist) (select l left (dec new-index) k)
        :else (select l (inc new-index) right (- k dist))))))

(defn percentile
  [s k]
  (let [scaled-k (int (* (count s) (/ k 100)))]
    (select (transient s) 0 (dec (count s)) scaled-k)))

