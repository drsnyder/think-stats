; Reference: http://www.itl.nist.gov/div898/handbook/eda/section3/eda35f.htm
(ns think-stats.chi-squared
  (:require (think-stats
              [util :as util]
              [homeless :as h]))
  (:import org.apache.commons.math3.distribution.ChiSquaredDistribution))

(defn chi-squared-statistic
  [observed expected]
  (assert (and (sequential? observed)
               (sequential? expected))
          "Error, the test statistic can only be computed on sequentials.")
  (assert (= (count observed) (count expected)) 
          "Error, observed and expected must be the same length.")
  (let [pairs (map vector observed expected)]
    (h/sum (for [[o e] pairs]
             (/ (h/square (- o e)) e)))))

(defn create-chi-squared
  [dof]
  (ChiSquaredDistribution. dof))

(def chi-squared (memoize create-chi-squared))

(defn p-value
  [dof x]
  (- 1.0 (.cumulativeProbability (chi-squared dof) x)))
