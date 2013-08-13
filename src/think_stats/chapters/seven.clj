(ns think-stats.chapters.seven
  (:require (think-stats [pregnancy :as preg]
                         [stats :as stats]
                         [cdf :as cdf]
                         [random :as random])))

(defn mean-difference
  "Helper function for reproducing the data in section 7.1 using."
  [edist-a edist-b n]
  (let [pool (vec (shuffle (concat edist-a edist-b)))
        size-a (count edist-a)
        size-b (count edist-b)
        cdfa (cdf/cdff (repeatedly size-a #(rand-nth pool)))
        cdfb (cdf/cdff (repeatedly size-b #(rand-nth pool)))
        mean-diff-edist (Math/abs (double (- (stats/mean edist-a) (stats/mean edist-b))))
        a-minus-b (repeatedly n
                              (fn []
                                (let [sample-a (cdf/sample-cdf cdfa size-a)
                                      sample-b (cdf/sample-cdf cdfb size-b)
                                      mean-diff-samples (double (- (stats/mean sample-a) (stats/mean sample-b)))]
                                  mean-diff-samples)))
        outside-mean-diff (filter #(>= mean-diff-edist (Math/abs %)) a-minus-b)
        outside (count outside-mean-diff)]
   {:mean-difference mean-diff-edist
    :outside (count outside-mean-diff)
    :p-value (double (/ outside n))}))

(defn pregnancy-mean-difference
  [column n]
  (let [[first-babies other all] (preg/load-data column)
        stats (mean-difference first-babies other n)]
    stats))

