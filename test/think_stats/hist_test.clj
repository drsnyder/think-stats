(ns think-stats.hist-test
  (:use [midje.sweet])
  (:require (think-stats [hist :as hist])))

(facts :seq->hist
  ((hist/seq->hist [1 2 3 4 4]) 4) => 2
  ((hist/seq->hist [1 2 3 4 4]) 1) => 1)

(facts :hist->mass
  (hist/hist->mass (hist/seq->hist (range 0 10))) => 10)

(facts :hist->mean
  (hist/hist->mean (hist/seq->hist (range 0 10))) => 9/2)
