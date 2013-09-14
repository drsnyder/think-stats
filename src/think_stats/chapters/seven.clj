(ns think-stats.chapters.seven
  (:require (think-stats [pregnancy :as preg]
                         [stats :as stats]
                         [cdf :as cdf]
                         [hist :as hist]
                         [random :as random]
                         [chi-squared :as chi-squared])))

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

(defn sample->mean-delta-dist
  "Returns [mean-of-means variance-of-means distribution]."
  [n edist-a size-a edist-b size-b]
  (let [mean-delta-dist
        (repeatedly n
                    (fn []
                      (let [sample-a (random/choice-seq size-a edist-a)
                            sample-b (random/choice-seq size-b edist-b)
                            [ma mb mean-diff-samples] (mean-difference sample-a sample-b)]
                        mean-diff-samples)))]
    [(stats/mean mean-delta-dist) (stats/variance mean-delta-dist) mean-delta-dist]))

(defn mean-difference-p-value
  [delta edist-a size-a edist-b size-b n]
  (let [delta (Math/abs delta)
        [mean-of-means var-of-means mean-delta-dist] (sample->mean-delta-dist n edist-a size-a edist-b size-b)
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
        j (prn (format "peha p-value %f" peh0))
        ; P(E) = P(E|Ha) * P(Ha) + P(E|H0) * P(H0)
        ; P(H0) = 1.0 - P(Ha)
        pe (+ (* peha pha) (* peh0 (- 1.0 pha))) ; pha is the prior
        posterior (/ (* pha peha) pe)]
    {:raw stats
     :posterior posterior}))


(defn pregnancy-power
  "Compute the power of a difference in mean hypothesis test for a pregnancy value between first born babies
  and other babies."
  [column sample-size alpha & {:keys []}]
  (let [[first-babies other all] (preg/load-data column)
        [mean-a mean-b delta] (mean-difference first-babies other)
        ; this may be bogus because we are using the variance from the same data set
        [mean-of-means var-of-means mean-delta-dist]
        (sample->mean-delta-dist sample-size
                                 (vec first-babies)
                                 (count first-babies)
                                 (vec other)
                                 (count other))
        ; compute the threshold-- values outside the threshold on a sample would result in rejecting the null
        ; hypothesis.
        threshold (cdf/normalicdf 0 (Math/sqrt var-of-means) (- 1.0 alpha))
        outside-mean-diff (filter #(>= (Math/abs %) threshold) mean-delta-dist)
        var-first (stats/variance first-babies)
        var-other (stats/variance other)]
    {:count-first (count first-babies)
     :count-other (count other)
     :mean-first (double mean-a)
     :var-first var-first
     :se-first (Math/sqrt (/ var-first (count first-babies)))
     :mean-other (double mean-b)
     :var-other var-other
     :se-other (Math/sqrt (/ var-other (count other)))
     :se (Math/sqrt (+ (/ var-first (count first-babies)) (/ var-other (count other))))
     :mean-of-means mean-of-means
     :var-of-means var-of-means
     :delta delta
     :threshold threshold
     :power (double (/ (count outside-mean-diff) sample-size)) }))



(defn dice-chi-squared-monte-carlo
  ; FIXME: make this more general
  "This is instructive, but unnecessary since the CDF of chi-squared distribution can be computed for any x
  to determine P(X <= x). It's interesting to run the simulation and see that it matches up with the continuous CDF."
  [dice-rolls horizon]
  (let  [sides 6
         dice #(random/fair-dice sides)
         expected (take sides (cycle [(* (/ 1 sides) dice-rolls)]))
         default (apply sorted-map (interleave  (range 1 sides)  (take sides  (cycle  [0]))))
         observations (map vals (map #(hist/hist % :dest default) (repeatedly horizon #(repeatedly dice-rolls dice))))]
    (map (comp double (partial chi-squared/chi-squared-statistic expected)) observations)))
