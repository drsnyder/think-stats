(ns think-stats.chapters.seven
  (:require (think-stats [pregnancy :as preg]
                         [stats :as stats]
                         [cdf :as cdf]
                         [random :as random])))

(defn mean-difference
  "Returns [meana meanb difference]"
  [colla collb]
  (let [meana (stats/mean colla)
        meanb (stats/mean collb)]
    [meana meanb (double (- meana meanb))]))


(defn mean-difference-p-value
  [delta edist-a size-a edist-b size-b n]
  (let [delta (Math/abs delta)
        mean-delta-dist
        (repeatedly n
                    (fn []
                      (let [sample-a (random/choice-seq size-a edist-a)
                            sample-b (random/choice-seq size-b edist-b)
                            [ma mb mean-diff-samples] (mean-difference sample-a sample-b)]
                        mean-diff-samples)))
        mean-of-means (stats/mean mean-delta-dist)
        var-of-means (stats/variance mean-delta-dist)
        j (prn (format "mean %2.5f var %2.5f of resampled deltas" mean-of-means var-of-means))
        outside-mean-diff (filter #(>= (Math/abs %) (Math/abs delta)) mean-delta-dist)
        outside (count outside-mean-diff)
        j (prn (format "min %f max %f" (apply min mean-delta-dist) (apply max mean-delta-dist)))
        cdf (cdf/cdff mean-delta-dist)
        left (cdf (* -1 delta))
        right (- 1 (cdf delta))
        p-value (+ left right)]
    {:mean-difference delta
     :outside outside
     :p-value-from-ratio (double (/ outside n))
     :left-p-value (double left)
     :right-p-value (double right)
     :p-value (double p-value)}))

(defn mean-difference-sim
  "Helper function for reproducing the data in section 7.1"
  [edist-a edist-b n]
  (let [pool (vec (concat edist-a edist-b))
        edist-a (vec edist-a)
        edist-b (vec edist-b)
        size-a (count edist-a)
        size-b (count edist-b)
        [meana meanb mean-diff-edist] (mean-difference edist-a edist-b)

        j (prn (format "delta %f" mean-diff-edist))
        ; the probability that the effect is real given the null hypothesis. we compute this by randomly
        ; sampling from the entire distribution (pool); X = (random/choice-seq pool size-a)
        ; Y = (random/choice-seq pool size-b)
        peh0 (mean-difference-p-value mean-diff-edist pool size-a pool size-b n)
        j (prn "calculated peh0")
        ; the probability that the effect is real given the hypothesis about the value. we compute this
        ; by randomly sampling X and Y from the data representing X and Y; X = (random/choice-seq edist-a size-a)
        ; Y = (random/choice-seq edist-b size-b)
        peha (mean-difference-p-value mean-diff-edist edist-a size-a edist-b size-b n)
        j (prn "calculated peha")
        ]
    {:peh0 peh0
     :peha peha}))

(defn pregnancy-mean-difference
  ; pha = prior
  [column n &{:keys [sample-size pha] :or {sample-size nil pha 0.5}}]
  (let [[first-babies other all] (preg/load-data column)
        mean (stats/mean all)
        var (stats/variance all)
        j (println (format "Mean %f var %f for %s of pooled data." (double mean) (double var) column))
        first-babies (if sample-size
                       (random/sample-seq sample-size first-babies)
                       first-babies)
        other (if sample-size
                (random/sample-seq sample-size other)
                other)
        stats (mean-difference-sim first-babies other n)
        peha (get-in stats [:peha :p-value])
        peh0 (get-in stats [:peh0 :p-value])
        ; P(E) = P(E|Ha) * P(Ha) + P(E|H0) * P(H0)
        ; P(H0) = 1.0 - P(Ha)
        pe (+ (* peha pha) (* peh0 (- 1.0 pha))) ; pha is the prior
        posterior (/ (* pha peha) pe)]
    {:raw stats
     :posterior posterior}))

