(ns think-stats.stats-test
  (:use [midje.sweet])
  (:require (think-stats [stats :as stats]
                         [hist :as hist]
                         [homeless :as h]
                         [util :as u])))

(facts :mean
       (stats/mean [0 1.0]) => 0.5
       (stats/mean [1 2.0]) => 1.5
       (hist/pmf->mean (hist/pmf [0 1.0])))

(facts :variance
       (stats/variance [0 1]) => 1/4
       (stats/variance [0 1] :sample true) => 1/2
       (hist/pmf->variance (hist/pmf [0 1])) => 1/4)



(facts :trim
       (stats/trim (range 1 11) 0.1) => '(2 3 4 5 6 7 8 9)
       (stats/trim (range 1 11) 0.5) => '()
       (stats/trim (range 1 101) 0.02) => (range 3 99)
       (stats/trim (range 1 101) 0.02 :left false) => (range 1 99)
       (stats/trim (range 1 101) 0.02 :right false) => (range 3 101))



(facts :t
       (let [alpha 0.5
             dof   120
             t (stats/alpha->t dof alpha)
             alpha' (stats/t->p-value dof t)]
         (h/approxiately-equal alpha alpha' 0.009)))
