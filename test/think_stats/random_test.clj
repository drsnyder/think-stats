(ns think-stats.random-test
  (:use [midje.sweet])
  (:require (think-stats 
              [constants :as c]
              [stats :as stats]
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

