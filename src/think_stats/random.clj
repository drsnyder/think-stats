(ns think-stats.random
  (:require (think-stats
              [distributions :as d]
              [constants :as c]
              [homeless :as h]
              [stats :as stats]))
  (:import org.apache.commons.math3.special.Erf))

(declare normalvariate)

(def standard-normal-mu 0)
(def standard-normal-sigma 1)
(def standard-normalvariate #(normalvariate standard-normal-mu standard-normal-sigma))

(def rankit-items 6)

(defn rankit-sample
  ([cdf]
   (into [] (repeatedly rankit-items cdf)))
  ([]
   (rankit-sample standard-normalvariate)))

(defn rankit-samples
  ([cdf n]
   (map stats/mean
        (apply map vector
               (map sort
                    (repeatedly n #(rankit-sample cdf))))))
  ([n]
   (rankit-samples standard-normalvariate n)))



(defn normalvariate
  "Generate random values from a normal distribution with mean mu and standard deviation sigma."
  [mu sigma]
  (let [p (rand)
        x (* c/sqrt2 (Erf/erfInv (- (* 2 p) 1)))]
    (+ (* sigma x) mu)))


