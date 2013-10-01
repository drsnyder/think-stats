(ns think-stats.chapters.eight
  (:require (think-stats
              [homeless :as h]
              [stats :as stats]
              [probability :as p]
              [hist :as hist]
              [random :as random]
              [estimation :as estimation])))

(defn vote
  [m]
  (let [event (p/event m)]
    (fn []
      (event (rand)))))

(comment
  (eight/vote-count-sim  {0 0.05 2 0.05 1 0.9} 1e6 1e6 10))

(defn vote-count-sim
  [prob-map a-votes b-votes iter]
  (let [v (vote prob-map)
        a (repeatedly iter #(h/sum (repeatedly a-votes v)))
        b (repeatedly iter #(h/sum (repeatedly b-votes v)))]
    (map vector a b)))


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

(comment
  ; problem 8.4
  ; h is the prior
  (def h (hist/uniform-pmf 0.1 1.5 100))
  ; evidence is the posterior
  (def evidence  [1.5, 2, 3, 4, 5, 12])
  ; the most likely
  (def prob-map
    (estimation/update-pmf
      h
      evidence
      ; what do we provide for lower and upper here?
      (partial likelihood  (fn [lambda x]
                             (cdf/cdf->probbability-range (partial cdf/expocdf lambda))))))
  (apply max-key val prob-map))

