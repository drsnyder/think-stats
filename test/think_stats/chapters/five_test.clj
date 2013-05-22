(ns think-stats.chapters.five-test
  (:use [midje.sweet])
  (:require (think-stats.chapters
              [five :as five])))


(facts :create-doors
       (count (five/create-doors)) => five/number-doors
       (count (filter (partial = :goat) (five/create-doors))) => 2
       (count (filter (partial = :car) (five/create-doors))) => 1)

(facts :remove-door
       (five/remove-door 0 [:a :b :c]) => [:b :c]
       (five/remove-door 1 [:a :b :c]) => [:a :c]
       (five/remove-door 2 [:a :b :c]) => [:a :b])

(facts :montys-pick-leaves
       (five/montys-pick-leaves [:goat :car]) => :car
       (five/montys-pick-leaves [:car :goat]) => :car
       (five/montys-pick-leaves [:goat :goat]) => :goat)



