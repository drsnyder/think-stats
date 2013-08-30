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


(defn mean-difference-sim
  "Helper function for reproducing the data in section 7.1 using."
  [edist-a edist-b n]
  (let [pool (vec (concat edist-a edist-b))
        size-a (count edist-a)
        size-b (count edist-b)
        [meana meanb mean-diff-edist] (mean-difference edist-a edist-b)
        mean-diff-edist (Math/abs mean-diff-edist)
        mean-delta-dist (repeatedly n
                              (fn []
                                (let [sample-a (random/choice-seq size-a pool)
                                      sample-b (random/choice-seq size-b pool)
                                      [ma mb mean-diff-samples] (mean-difference sample-a sample-b)]
                                  mean-diff-samples)))
        mean-of-means (stats/mean mean-delta-dist)
        var-of-means (stats/variance mean-delta-dist)
        j (prn (format "mean %2.3f var %2.3f of resampled deltas" mean-of-means var-of-means))
        outside-mean-diff (filter #(>= (Math/abs %) mean-diff-edist) mean-delta-dist)
        outside (count outside-mean-diff)
        cdf (cdf/cdff mean-delta-dist)
        left (cdf (* -1 mean-diff-edist))
        right (- 1 (cdf mean-diff-edist))
        p-value (+ left right)]
   {:mean-difference mean-diff-edist
    :outside outside
    :p-value-from-ratio (double (/ outside n))
    :left-p-value (double left)
    :right-p-value (double right)
    :p-value (double p-value)}))

(defn pregnancy-mean-difference
  [column n &{:keys [sample-size] :or [sample-size nil]}]
  (let [[first-babies other all] (preg/load-data column)
        first-babies (if sample-size
                       (random/sample-seq sample-size first-babies)
                       first-babies)
        other (if sample-size
                (random/sample-seq sample-size other)
                other)
        stats (mean-difference-sim first-babies other n)]
    stats))

