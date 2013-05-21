(ns think-stats.stats-test
  (:use [midje.sweet])
  (:require (think-stats [stats :as stats]
                         [util :as u])))

(facts :mean
       (stats/mean [0 1.0]) => 0.5
       (stats/mean [1 2.0]) => 1.5
       (stats/pmf->mean (stats/pmf [0 1.0])))

(facts :variance
       (stats/variance [0 1]) => 1/4
       (stats/variance [0 1] :sample true) => 1/2
       (stats/pmf->variance (stats/pmf [0 1])) => 1/4)


(facts :pmf
       ((stats/pmf [1 2 3 4]) 2) => 1/4
       ((stats/pmf [1 2 2 3 4]) 2) => 2/5)

(facts :hist
       ((stats/hist [1 2 3 4 4]) 4) > 2
       ((stats/hist [1 2 3 4 4]) 1) > 1)

(facts :trim 
       (stats/trim (range 1 11) 0.1) => '(2 3 4 5 6 7 8 9)
       (stats/trim (range 1 11) 0.5) => '()
       (stats/trim (range 1 101) 0.02) => (range 3 99)
       (stats/trim (range 1 101) 0.02 :left false) => (range 1 99)
       (stats/trim (range 1 101) 0.02 :right false) => (range 3 101))

(facts :lifetime
       ((stats/pmf->remaining-lifetime (stats/pmf [0 1 1 2 2])) 2) => 2/5
       ((stats/pmf->remaining-lifetime (stats/pmf [0 1 1 2 2])) 1) => 4/5
       ((stats/pmf->remaining-lifetime (stats/pmf [0 1 1 2 2])) 0) => 5/5)

(facts :binning
       (stats/bin-pmf-freq (stats/pmf [1 1 1 2 2 2 3 3 3]) #{2 3}) => 6/9)
