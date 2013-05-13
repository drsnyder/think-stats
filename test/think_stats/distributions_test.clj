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

(facts :cdf
       (let [cdf (d/cdf (range 1 6))]
         (get cdf 0) => nil
         (get cdf 1) => 1/5
         (get cdf 2) => 2/5
         (get cdf 3) => 3/5
         (get cdf 4) => 4/5
         (get cdf 5) => 1
         (get cdf 6) => nil))

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

(facts :exponential
       (let [lambda 2
             sample (repeatedly 100000 (fn [] (d/expovariate lambda)))
             median (d/percentile sample 50)
             mean (stats/mean sample)]
         (h/approxiately-equal median (d/expomedian lambda)) => true
         (h/approxiately-equal mean (d/expomean lambda)) => true))


(facts :pareto :slow
       (let [alpha 1
             threshold 0.5
             sample (repeatedly 100000 (fn [] (d/paretovariate alpha threshold)))
             cdf (d/cdff sample :to-float true)
             ccdf (fn [x] (- 1 (cdf x)))
             x1 0
             y1 (Math/log (ccdf (Math/exp x1)))
             x2 1
             y2 (Math/log (ccdf (Math/exp x2)))]
         ; intercept at alpha * Log(threshold)
         (h/approxiately-equal (Math/log (ccdf (Math/exp 0))) (* alpha (Math/log threshold))) => true
         ; slope at -alpha
         (h/approxiately-equal (/ (- x2 x1) (- y2 y1)) (* -1 alpha)) => true)


       (dorun
         (for [alpha (range 2 10)
               :let [x-min 1
                     sample (repeatedly 100000 (fn [] (d/paretovariate alpha x-min)))
                     median (d/percentile sample 50)
                     mean   (stats/mean sample)]]
           (and
             (h/approxiately-equal median (d/paretomedian alpha x-min) 0.05)
             (h/approxiately-equal mean (d/paretomean alpha x-min) 0.05)) => true)))

(facts :normal :slow
       (let [mu 5.0
             sigma 1.0
             threshold 0.01
             sample (repeatedly 1000000 (fn [] (d/normalvariate mu sigma)))
             mean (stats/mean sample)
             stddev (stats/stddev sample)]
         (h/approxiately-equal mean 5.0) => true
         (h/approxiately-equal stddev 1.0) => true))
