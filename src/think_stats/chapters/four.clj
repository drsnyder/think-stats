(ns think-stats.chapters.four
  (:require (think-stats [stats :as stats]
                         [util :as util]
                         [survey :as s]
                         [homeless :as h]
                         [distributions :as d])
            [clj-http.client :as client]))

; TODO: confirm that this has the slope and intesect that we expect
(defn pareto-cdf
  "4.3 Plots the pareto CDF and CCDF. The CCDF is plotted on a log-log scale. See also plots/pareto.R.
  Example: 
  (four/pareto-cdf 10000 1 0.5 0 10 0.1) 
  "
  [n alpha threshold x-min x-max step]
  (let [r-script "plots/pareto.R"
        csv-out "plots/pareto.csv"
        p (repeatedly n (fn [] (d/paretovariate alpha threshold)))
        cdf (d/cdff p :to-float true)
        xs (range x-min x-max step)
        ys (map cdf xs)
        csv (cons (list "x" "y") (map list xs ys))]
    (util/write-to-csv csv-out csv)
    (let [ret (util/shell-exec (format "Rscript %s %s %s %s" r-script csv-out alpha threshold))]
      (when (not= (:exit ret) 0)
        (println "Error: " (:err ret)))
      {:summary (stats/summary p)
       :data p})))


(defn exponential-cdf
  "4.1 Plots the exponential CDF and CCDF. See plots/exponential.R.
  Example:
  (four/exponential-cdf 10000 2 0 2.5 0.1)
  "
  [n lambda x-min x-max step]
  (let [r-script "plots/exponential.R"
        csv-out "plots/exponential.csv"
        e (repeatedly n (fn [] (d/expovariate lambda)))
        cdf (d/cdff e :to-float true)
        xs (range x-min x-max step)
        ys (map cdf xs)
        csv (cons (list "x" "y") (map list xs ys))]
    (util/write-to-csv csv-out csv)
    (let [ret (util/shell-exec (format "Rscript %s %s %s" r-script csv-out lambda))]
      (when (not= (:exit ret) 0)
        (println "Error: " (:err ret)))
      {:summary (stats/summary e)
       :data e})))


(defn extract-features
  "4.5 Get Anna Karenina, by Leo Tolstoy from here http://www.gutenberg.org/files/1399/1399-0.txt."
  [corpus &{:keys [min-len] :or {min-len 1}}]
  (let [words (clojure.string/split corpus #"\s+")]
    (->> words
         (map clojure.string/lower-case)
         (map #(clojure.string/replace % #"[^a-z]+" "") )
         (filter #(> (count %) min-len)))))

(defn corpus->zifs
  "Given a corpus, extract the features, frequencies, and cdf. Returns a map with those keys.

  $ curl http://www.gutenberg.org/files/1399/1399-0.txt > tmp/ak.txt
  (def corpus (slurp \"tmp/ak.txt\"))

  ; CDF
  (plots/line (keys cdf) (map float (vals cdf))) 

  ; CCDF on log-log 
  (plots/line (map log (keys cdf)) (map #(log (- 1 %)) (map float (vals cdf)))) 
  "
  [corpus]
  (let [features (extract-features corpus)
        freq (frequencies features)
        ; since these are discrete values, dedup the frequencies
        cdf (d/cdf (distinct (vec (vals freq))))]
    ; TODO: compute the x intercept and the slope
    {:cdf cdf
     :features features
     :frequencies freq}))

