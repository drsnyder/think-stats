(ns think-stats.stats-test
  (:use [midje.sweet])
  (:require (think-stats [stats :as stats]
                         [util :as u])))

(facts :mean
       (stats/mean (take 100 (cycle [0 1.0]))) => 0.5
       (stats/mean [1 2.0]) => 1.5)

(facts :pmf
       ((stats/pmf [1 2 3 4]) 2) => 1/4
       ((stats/pmf [1 2 2 3 4]) 2) => 2/5)
