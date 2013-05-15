(ns think-stats.random
  (:require (think-stats
              [distributions :as d]
              [homeless :as h]
              [stats :as stats])))

(def rankit-items 6)

(defn rankit-samples
  ([cdf n]
   (repeatedly rankit-items 
     #(repeatedly n cdf)))
  ([n]
   (rankit-samples (fn [] (d/normalvariate 0 1)) n)))

(defn rankit-samples->means
  [rankit-samples]
  (map stats/mean rankit-samples))


              
