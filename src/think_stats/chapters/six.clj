(ns think-stats.chapters.six
  (:require (think-stats
              [random :as random]
              [stats :as stats]
              [util :as util]
              [plots :as plots]
              [homeless :as h])))

(defn sample-mean
  "Compute the mean of a sample of size n."
  [n X]
  (stats/mean (random/sample n X) n))

(defn sample-mean-stats
  "Compute k means of samples of size n from random variable X."
  [k n X]
  (let [s (random/sample k #(sample-mean n X))
        mean (stats/mean s k)
        var (stats/mean-variance s h/square n mean)]
    [mean var]))

(defn sample-mean-probability-plot
  "Generate normal probability plot for the given sample means.
  Example:
  (six/sample-mean-probability-plot
    (random/sample 1000
                   #(six/sample-mean (Math/pow 2 16)
                   test-dist)) 1000
                   \"Lognormal mu = 1 sigma = 2 n = 2^16 samples = 1000\")"
  [sample-means n dist-desc]
  (let [sample-means (sort sample-means)
        normal-points (sort (random/sample n random/standard-normalvariate))]
    (plots/line normal-points sample-means
                :title (format "Normal Probability Plot for %s" dist-desc))))
