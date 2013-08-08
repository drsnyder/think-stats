(ns think-stats.chapters.six
  (:require (think-stats
              [random :as random]
              [stats :as stats]
              [util :as util]
              [homeless :as h])))

(defn sample-mean
  "Compute the mean of a sample of size n."
  [n X]
  (stats/mean (random/sample n X) n))

(defn sample-mean-stats
  "Compute k means of samples of size n from random variable X."
  [k n X]
  (let [s (repeatedly k #(sample-mean n X))
        mean (stats/mean s n)
        var (stats/mean-variance s h/square n mean)]
    [mean var]))
