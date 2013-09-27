(ns think-stats.chapters.eight
  (:require (think-stats
              [homeless :as h]
              [stats :as stats]
              [probability :as p]
              [hist :as hist]
              [random :as random])))

(defn vote
  [m]
  (let [event (p/event m)]
    (fn []
      (event (rand)))))

(comment
  (eight/vote-count-sim  {0 0.05 2 0.05 1 0.9} 1e6 1e6 10))

(defn vote-count-sim
  [prob-map a-votes b-votes iter]
  (let [v (vote prob-map)
        a (repeatedly iter #(h/sum (repeatedly a-votes v)))
        b (repeatedly iter #(h/sum (repeatedly b-votes v)))]
    (map vector a b)))

;(defn vote-sim
  ;[cdf count-a count-b iter]
  ;(let [a]))


