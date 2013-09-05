(ns think-stats.chapters.seven
  (:require (think-stats [pregnancy :as preg]
                         [stats :as stats]
                         [cdf :as cdf]
                         [random :as random])))

(defn random-partition
  [s n]
  (let [rs (shuffle s)]
    [(take n rs) (drop n rs)]))

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
        j (prn (format "delta: %f mean %2.5f var %2.5f of resampled deltas" delta mean-of-means var-of-means))
        outside-mean-diff (filter #(>= (Math/abs %) (Math/abs delta)) mean-delta-dist)
        outside (count outside-mean-diff)
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
  [pool edist-a edist-b iter &{:keys [partition-dist] :or {partition-dist false}}]
  (let [size-a (count edist-a)
        size-b (count edist-b)
        [edist-a model-a] (if partition-dist
                            (random-partition edist-a (/ size-a 2))
                            [edist-a edist-a])
        [edist-b model-b] (if partition-dist
                            (random-partition edist-b (/ size-b 2))
                            [edist-b edist-b])
        pool (if partition-dist
               (vec (concat model-a model-b))
               (vec pool))
        [edist-a edist-b model-a model-b] (map vec (list edist-a edist-b model-a model-b))
        mean (stats/mean pool)
        var (stats/variance pool)
        j (println (format "Mean %f var %f of pooled data." (double mean) (double var)))
        [meana meanb delta] (mean-difference edist-a edist-b)
        j (println (format "Mean a %f mean b %f delta %f" (double meana) (double meanb) delta))
        ; the probability that the effect is real given the null hypothesis. we compute this by randomly
        ; sampling from the entire distribution (pool); X = (random/choice-seq pool size-a)
        ; Y = (random/choice-seq pool size-b)
        peh0 (mean-difference-p-value delta pool size-a pool size-b iter)
        j (prn "calculated peh0")
        ; the probability that the effect is real given the hypothesis about the value. we compute this
        ; by randomly sampling X and Y from the data representing X and Y; X = (random/choice-seq edist-a size-a)
        ; Y = (random/choice-seq edist-b size-b)
        peha (mean-difference-p-value delta model-a (count model-a) model-b (count model-b) iter)
        j (prn "calculated peha")]
    {:peh0 peh0
     :peha peha}))

(comment (seven/pregnancy-mean-difference "totalwgt_oz" 1000))

; TODO: support partitioning the data
(defn pregnancy-mean-difference
  ; pha = prior
  [column n &{:keys [sample-size pha partition-dist] :or {sample-size nil pha 0.5 partition-dist false}}]
  (let [[first-babies other all] (preg/load-data column)
        first-babies (if sample-size
                       (random/sample-seq sample-size first-babies)
                       first-babies)
        other (if sample-size
                (random/sample-seq sample-size other)
                other)
        stats (mean-difference-sim all first-babies other n :partition-dist partition-dist)
        peha (get-in stats [:peha :p-value])
        peh0 (get-in stats [:peh0 :p-value])
        ; P(E) = P(E|Ha) * P(Ha) + P(E|H0) * P(H0)
        ; P(H0) = 1.0 - P(Ha)
        pe (+ (* peha pha) (* peh0 (- 1.0 pha))) ; pha is the prior
        posterior (/ (* pha peha) pe)]
    {:raw stats
     :posterior posterior}))

