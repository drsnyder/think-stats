(ns think-stats.probability-test
  (:use [midje.sweet])
  (:require (think-stats [probability :as p])))

(facts :n-choose-k
       (p/n-choose-k 0 0) => 1
       (p/n-choose-k 1 0) => 1
       (p/n-choose-k 1 1) => 1
       (p/n-choose-k 1 2) => 0
       (p/n-choose-k 2 1) => 2
       (p/n-choose-k 2 2) => 1
       (p/n-choose-k 100 2) => 4950)
