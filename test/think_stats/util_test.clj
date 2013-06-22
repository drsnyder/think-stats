(ns think-stats.util-test
  (:use [midje.sweet])
  (:require (think-stats [util :as u])))

(facts :find-streak
       (u/contains-streak? [1 2] 1 3) => nil
       (u/contains-streak? [1 2 3] 1 3) => nil
       (u/contains-streak? [1 2 2] 2 3) => nil
       (u/contains-streak? [2 2 3] 2 3) => nil
       (u/contains-streak? [3 2 2] 2 3) => nil
       (u/contains-streak? [3 2 2] 2 2) => true
       (u/contains-streak? [2 2 3] 2 2) => true
       (u/contains-streak? [2 3 2] 2 2) => nil
       (u/contains-streak? [2 2 3] 2 2) => true
       (u/contains-streak? (concat (take 1000000 (cycle [0])) (list 1)) 1 2) => nil
       (u/contains-streak? (concat (take 1000000 (cycle [0])) (list 1 1)) 1 2) => true)
