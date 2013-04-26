(ns think-stats.distributions-test
  (:use [midje.sweet])
  (:require (think-stats [stats :as stats]
                         [homeless :as h]

                         [distributions :as d])))

(facts :percentile
      ; percentile (using the selection algorithm should function the same 
      ; as the standard version for 1..100
      (= (for [i (range 1 101)] (d/percentile (range 1 101) i))
         (for [i (range 1 101)] (d/percentile-w (range 1 101) i))) => true)

(facts :cddf
      (let [cdf (d/cdff (range 1 6))]
        (cdf 0) => 0
        (cdf 1) => 1/5
        (cdf 2) => 2/5
        (cdf 2.5) => 3/5
        (cdf 3) => 3/5
        (cdf 4) => 4/5
        (cdf 5) => 1
        (cdf 6) => 1)

      (let [cdf (d/cdff (range 1 101))]
        (cdf 10) => 1/10
        (cdf 0.1 :value) => 10))

(facts :pareto :slow
      (dorun
        (for [alpha (range 2 10)
              :let [x-min 1
                    sample (repeatedly 100000 (fn [] (d/paretovariate alpha x-min)))
                    median (d/percentile sample 50)
                    mean   (stats/mean sample)]]
          (and
            (h/approxiately-equal median (d/paretomedian alpha x-min) 0.05)
            (h/approxiately-equal mean (d/paretomean alpha x-min) 0.05)) => true)))

