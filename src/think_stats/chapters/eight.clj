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
  [prob-map votes iter]
  (let [v (vote prob-map)]
    (repeatedly iter #(h/sum (repeatedly votes v)))))



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
  (def h (hist/uniform-pmf 0.001 1.5 100))
  ; evidence is the posterior
  (def evidence  [1.5, 2, 3, 4, 5, 12])
  ; the most likely
  (def prob-map
    (estimation/update-pmf
      h
      evidence
      ; what do we provide for lower and upper here?
      (partial estimation/likelihood  (fn [lambda x]
                             (/ (* lambda (Math/exp (* x (* -1 lambda))))
                                (cdf/cdf->probability-range (partial cdf/expocdf lambda) 1.0 20.0))))))
  (apply max-key val prob-map)
  (def m  (into  (sorted-map) prob-map))
  (util/write-to-csv "tmp/decay.csv"  (concat  [["parameter" "posterior"]]  (map vector  (keys m)  (vals m)))) 
  ;R decay = read.csv("decay.csv", header = T)
  ;R ggplot(decay) + geom_line(aes(x=parameter,y=posterior))
  )

(comment
  ; section 8.9
  ; question 1: MLE seems to be i or 60. P(E|N) goes down proportional to N as you go up from i
  ; question 2: the value of i that minimizes MSE is 1 (apply min-key val (into (sorted-map) (for [a (range 1 200)] [a (stats/mean-error a [(* a 60)] 1)])))
  ; question 3: i think this is also 1. if you increase a then your error goes up.
  ; question 4: (/ (reduce + (range 1 120)) 119) = 2X - 1 = 119
  ; question 5:
  (def h (hist/uniform-pmf 1 200 200))
  (def posterior-map
    (estimation/update-pmf
      h
      [60]
      (partial estimation/likelihood
               (fn [num-trains train-seen]
                 (if (< num-trains train-seen)
                   0
                   (/ 1.0 num-trains))))))
  (apply max-key val posterior-map)
  (def m  (into  (sorted-map) posterior-map))
  (def cdf (into  (sorted-map-by  (fn  [k1 k2]  (compare  [(get posterior-map k1) k1]  [(get posterior-map k2) k2]))) posterior-map)))
