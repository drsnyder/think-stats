(ns think-stats.random-test
  (:use [midje.sweet])
  (:require (think-stats
              [constants :as c]
              [distributions :as d]
              [stats :as stats]
              [cdf :as cdf]
              [homeless :as h]
              [random :as random])))


(facts :normal :slow
       (let [mu 5.0
             sigma 1.0
             threshold 0.01
             sample (repeatedly 1000000 (fn [] (random/normalvariate mu sigma)))
             mean (stats/mean sample)
             stddev (stats/stddev sample)]
         (h/approxiately-equal mean 5.0) => true
         (h/approxiately-equal stddev 1.0) => true)
       (let [mu 0
             sigma 1
             threshold 0.01
             sample (repeatedly 1000000 (fn [] (random/normalvariate mu sigma)))
             mean (stats/mean sample)
             stddev (stats/stddev sample)]
         (h/approxiately-equal mean 0.0) => true
         (h/approxiately-equal stddev 1.0) => true))

(facts :normal :rankits
       (every? true? 
               (map #(h/approxiately-equal (first %) (second %) 0.1)
                    (map vector (random/rankit-samples 1000) 
                         c/standard-normal-order-statistics))) => true)

(facts :exponential
       (let [lambda 2
             sample (repeatedly 100000 (fn [] (random/expovariate lambda)))
             median (d/percentile sample 50)
             mean (stats/mean sample)]
         (h/approxiately-equal median (random/expomedian lambda)) => true
         (h/approxiately-equal mean (random/expomean lambda)) => true))

(facts :pareto :slow
       (let [alpha 1
             threshold 0.5
             sample (repeatedly 100000 (fn [] (random/paretovariate alpha threshold)))
             cdf (cdf/cdff sample)
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
                     sample (repeatedly 100000 (fn [] (random/paretovariate alpha x-min)))
                     median (d/percentile sample 50)
                     mean   (stats/mean sample)]]
           (and
             (h/approxiately-equal median (random/paretomedian alpha x-min) 0.05)
             (h/approxiately-equal mean (random/paretomean alpha x-min) 0.05)) => true)))
