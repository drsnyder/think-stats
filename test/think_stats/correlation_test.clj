(ns think-stats.correlation-test
  (:use [midje.sweet])
  (:require (think-stats [stats :as stats]
                         [homeless :as h]
                         [correlation :as cor]
                         [util :as u])))

(facts :cov
  (let [x (repeatedly 1000 #(rand-int 100))]
    (cor/cov x x) => (stats/variance x)))

(facts :pearsons
  (let [x (repeatedly 1000 #(rand-int 100))]
    (cor/pearsons-correlation x x) => (roughly 1.0)))

(facts :spearmans
  (let [x (repeatedly 1000 #(rand-int 100))]
    (cor/spearmans-rank-correlation x x) => (roughly 1.0)))

