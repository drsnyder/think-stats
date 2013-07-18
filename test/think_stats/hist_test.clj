(ns think-stats.hist-test
  (:use [midje.sweet])
  (:require (think-stats [hist :as hist])))

(facts :seq->hist
  ((hist/hist [1 2 3 4 4]) 4) => 2
  ((hist/hist [1 2 3 4 4]) 1) => 1)

(facts :hist->mass
  (hist/hist->mass (hist/hist (range 0 10))) => 10)

(facts :hist->mean
  (hist/hist->mean (hist/hist (range 0 10))) => 9/2)

(facts :pmf
       ((hist/pmf [1 2 3 4]) 2) => 1/4
       ((hist/pmf [1 2 2 3 4]) 2) => 2/5)


(facts :lifetime
       ((hist/pmf->remaining-lifetime (hist/pmf [0 1 1 2 2])) 2) => 2/5
       ((hist/pmf->remaining-lifetime (hist/pmf [0 1 1 2 2])) 1) => 4/5
       ((hist/pmf->remaining-lifetime (hist/pmf [0 1 1 2 2])) 0) => 5/5)


(facts :binning
       (hist/bin-pmf-freq (hist/pmf [1 1 1 2 2 2 3 3 3]) #{2 3}) => 6/9)
