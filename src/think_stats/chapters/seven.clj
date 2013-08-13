(ns think-stats.chapters.seven
  (:require (think-stats [pregnancy :as preg]
                         [stats :as stats]
                         [cdf :as cdf]
                         [random :as random])))

(defn mean-difference
  "Helper function for reproducing the data in section 7.1 using."
  [edist-a edist-b n]
  (let [pool (vec (concat edist-a edist-b))
        size-a (count edist-a)
        size-b (count edist-b)
        mean-diff-edist (Math/abs (double (- (stats/mean edist-a) (stats/mean edist-b))))
        a-minus-b (repeatedly n
                              (fn []
                                (let [sample-a (random/sample-seq size-a pool)
                                      sample-b (random/sample-seq size-b pool)
                                      mean-diff-samples (double (- (stats/mean sample-a) (stats/mean sample-b)))]
                                  mean-diff-samples)))
        outside-mean-diff (filter #(>= (Math/abs %) mean-diff-edist) a-minus-b)
        outside (count outside-mean-diff)]
   {:mean-difference mean-diff-edist
    :outside outside
    :p-value (double (/ outside n))}))

(defn pregnancy-mean-difference
  [column n]
  (let [[first-babies other all] (preg/load-data column)
        stats (mean-difference first-babies other n)]
    stats))

