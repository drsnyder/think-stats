(ns think-stats.stats-test
  (:use [midje.sweet])
  (:require (think-stats [stats :as stats]
                         [util :as u])))

(facts :mean
       (stats/mean (take 100 (cycle [0 1.0]))) => 0.5
       (stats/mean [1 2.0]) => 1.5)
