(ns think-stats.probability-test
  (:use [midje.sweet])
  (:require (think-stats [probability :as p]
                         [homeless :as h])))

(facts :n-choose-k
       (p/n-choose-k 0 0) => 1
       (p/n-choose-k 1 0) => 1
       (p/n-choose-k 1 1) => 1
       (p/n-choose-k 1 2) => 0
       (p/n-choose-k 2 1) => 2
       (p/n-choose-k 2 2) => 1
       (p/n-choose-k 100 2) => 4950)

(facts :slow :event
  (let [e (p/event  {0 0.05 2 0.05 1 0.9})
        s (repeatedly 1e6 #(e (rand)))]
    (h/approximately-equal (/ (count (filter #(= 1 %) s)) 1.0e6), 0.9)  => true
    (h/approximately-equal (/ (count (filter #(= 0 %) s)) 1.0e6), 0.05) => true
    (h/approximately-equal (/ (count (filter #(= 2 %) s)) 1.0e6), 0.05) => true))
