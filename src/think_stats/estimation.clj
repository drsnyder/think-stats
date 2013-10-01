(ns think-stats.estimation
  "Bayesian estimation."
  (:require (think-stats [random :as random]
                         [hist :as hist])))

(defn update-pmf
  [pmf evidence f]
  ; normalize the compute the posterior distributions
  ; the normalization factor is sum(P(Hi)P(X|Hi)) which are all
  ; of the values in the PMF
  (hist/normalize-pmf
    (reduce #(update-in %1 [%2]
                        (fn [h]
                          ; h is the initial probability
                          (* h (f evidence %2))))
            pmf
            (keys pmf))))

(defn likelihood
  "P(X|Hi) which is the chance of drawing the sample X given Hi."
  [f evidence Hi]
  (assert (sequential? evidence))
  ; product of sequence f(Hi,xj)
  (reduce * (map (partial f Hi) evidence)))
