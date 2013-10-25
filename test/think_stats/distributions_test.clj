(ns think-stats.distributions-test
  (:use [midje.sweet])
  (:require (think-stats [stats :as stats]
                         [homeless :as h]
                         [distributions :as d])))

(facts :percentile
      ; percentile (using the selection algorithm should function the same 
      ; as the standard version for 1..100
      (= (for [i (range 1 101)] (d/percentile (range 1 101) i))
         (for [i (range 1 101)] (d/percentile-w (range 1 101) i))) => true)

(facts :average-ranks
  (d/average-ranks 1 1) => 1
  (d/average-ranks 2 2) => 1/2
  (d/average-ranks 3 2) => 3/2
  (d/average-ranks 10 3) => 8)

(facts :rank-seq
  (let [l [7 1 2 5]
        l2 [1 1 2 3 3 4 5 5 5]]
    (d/rank-seq l) => (list 4 1 2 3)
    (d/rank-seq l2) => (list 3/2 3/2 3 9/2 9/2 6 8 8 8)))
