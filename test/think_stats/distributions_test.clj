(ns think-stats.stats-test
  (:use [midje.sweet])
  (:require (think-stats [stats :as stats]
                         [distributions :as d])))

(fact :distributions
      ; percentile (using the selection algorithm should function the same 
      ; as the standard version for 1..100
      (= (for [i (range 1 101)] (d/percentile (range 1 101) i))
         (for [i (range 1 101)] (d/percentile-w (range 1 101) i)) => true))

