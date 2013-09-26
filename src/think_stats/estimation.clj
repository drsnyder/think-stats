(ns think-stats.estimation
  "Bayesian estimation."
  (:require (think-stats [random :as random]
                         [hist :as hist])))

(comment
  ; section 8.7
  ; h is the prior
  (def h (hist/uniform-pmf 0.5 1.5 10))
  ; evidence is the posterior
  (def evidence  [2.675, 0.198, 1.152, 0.787, 2.717, 4.269])
  ; the most likely
  (def prob-map
    (estimation/update-pmf h evidence (partial likelihood random/expopdf)))
  (apply max-key val prob-map))

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
