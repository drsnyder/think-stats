(ns think-stats.cdf-test
  (:use [midje.sweet])
  (:require (think-stats [cdf :as cdf]
                         [random :as random]))
  (:import [think-stats.types]))

(facts :cdf
       (let [cdf (cdf/cdf (range 1 6))]
         (get cdf 0) => nil
         (get cdf 1) => 1/5
         (get cdf 2) => 2/5
         (get cdf 3) => 3/5
         (get cdf 4) => 4/5
         (get cdf 5) => 1
         (get cdf 6) => nil))

(facts :cdf->pmf
  (let [cdf (cdf/cdf (range 1 10))
        pmf (cdf/cdf->pmf cdf)]
    (get pmf 1) => 1/9
    (get pmf 9) => 1/9))

(facts :cddf
      (let [cdf (cdf/cdff (range 1 6))]
        (cdf 0)   => 0
        (cdf 1)   => 1/5
        (cdf 2)   => 2/5
        (cdf 2.5) => 3/5
        (cdf 3)   => 3/5
        (cdf 4)   => 4/5
        (cdf 5)   => 1
        (cdf 6)   => 1)

      (let [cdf (cdf/cdff (range 1 101))]
        (cdf 10) => 1/10
        (cdf 0.1 :value) => 10))
