(ns think-stats.stats-test
  (:use [midje.sweet])
  (:require (think-stats [homeless :as h]
                         [util :as u])))


(facts :bisect
       (bisect (range 0 10) 0 :right) => 1
       (bisect (range 0 10) 0 :left) => 0

       (bisect (range 0 10) 1 :right) => 2
       (bisect (range 0 10) 1 :left) => 1

       (bisect (range 0 10) 2 :right) => 3
       (bisect (range 0 10) 2 :left) => 2

       (bisect (range 0 10) 10 :right) => 10
       (bisect (range 0 10) 10 :left) => 10)
