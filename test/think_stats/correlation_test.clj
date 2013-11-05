(ns think-stats.correlation-test
  (:use [midje.sweet])
  (:require (think-stats [stats :as stats]
                         [homeless :as h]
                         [random :as random]
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

(facts :least-squares
  ; use model fitting of a Weibull distribution to test the correct resolution
  ; of the shape and scale parameters from a liner transformation.
  (let [k 0.5
        lambda 1.0
        w (repeatedly 100000 #(random/weibullvariate k lambda))
        [x y] (random/weibull-line w)
        model (cor/least-squares x y)]
    ; the slope should be k
    (:slope model) => (roughly k 0.1)
    ; the intercept should be 0
    (:intercept model) => (roughly (* k (Math/log lambda)) 0.006)))

