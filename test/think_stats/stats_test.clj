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

(facts :lifetime
       ((stats/pmf->remaining-lifetime (stats/pmf [0 1 1 2 2])) 2) => 2/5
       ((stats/pmf->remaining-lifetime (stats/pmf [0 1 1 2 2])) 1) => 4/5
       ((stats/pmf->remaining-lifetime (stats/pmf [0 1 1 2 2])) 0) => 5/5)
