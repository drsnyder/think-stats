(ns think-stats.homeless-test
  (:use [midje.sweet])
  (:require (think-stats [homeless :as h]
                         [util :as u])))


(facts :bisect
       (h/bisect (range 0 10) 0 :right) => 1
       (h/bisect (range 0 10) 0 :left) => 0

       (h/bisect (range 0 10) 1 :right) => 2
       (h/bisect (range 0 10) 1 :left) => 1

       (h/bisect (range 0 10) 2 :right) => 3
       (h/bisect (range 0 10) 2 :left) => 2

       (h/bisect (range 0 10) 10 :right) => 10
       (h/bisect (range 0 10) 10 :left) => 10)

(facts :dissoc-vec
  (let [v [1 2 3]]
    (h/dissoc-vec v 0) => [2 3]
    (h/dissoc-vec v 1) => [1 3]
    (h/dissoc-vec v 2) => [1 2]))
