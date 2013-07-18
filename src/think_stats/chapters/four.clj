(ns think-stats.chapters.four
  (:require (think-stats
              [random :as random]
              [stats :as stats]
              [hist :as hist]
              [cdf :as cdf]
              [util :as util]
              [survey :as s]
              [homeless :as h]
              [plots :as plots]
              [brfss :as brfss]
              [distributions :as d])
            [clojure.data.csv :as csv]
            [clj-http.client :as client]
            [clojure.java.io :as io]))

; TODO: confirm that this has the slope and intesect that we expect
(defn pareto-cdf
  "4.3 Plots the pareto CDF and CCDF. The CCDF is plotted on a log-log scale. See also plots/pareto.R.
  Example:
  (four/pareto-cdf 10000 1 0.5 0 10 0.1)
  "
  [n alpha threshold x-min x-max step]
  (let [r-script "plots/pareto.R"
        csv-out "plots/pareto.csv"
        p (repeatedly n (fn [] (random/paretovariate alpha threshold)))
        cdf (cdf/cdff p)
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
        e (repeatedly n (fn [] (random/expovariate lambda)))
        cdf (cdf/cdff e)
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
        cdf (h/map-map float (cdf/cdf (distinct (vec (vals freq)))) :dest (sorted-map))]
    ; TODO: compute the x intercept and the slope
    {:cdf cdf
     :features features
     :frequencies freq}))

; The probability that someone has an IQ of 190 (mean = 100 & stddev = 15)
; is the cumulative area beyond 6 standard deviations under the normal curve.
; We can compute the area under the normal curve up-to 6 standard deviations so
; to compute the value we are looking for we subtract it from 1.
;
; p = 0.9999999990134123
; 6B * p = 6 people
(def langans (h/round (* 6000000000 (* (- 1 (stats/z->area 6))))))


(defn running-speeds-normal-probability-plot
  "Generates the normal probability plot for the running speeds from chapter three.
  (running-speeds-normal-probability-plot (three/get-speeds))"
  [speeds]
  (let [speeds (sort speeds)
        speed-sample (sort (repeatedly (count speeds) random/standard-normalvariate))]
    (plots/line speed-sample speeds)))

(defn brfss-4.11-gen-data
  [file]
  (let [data (brfss/load-data file)
        weights (map #(get % "weight2") data)
        ; we end up sorting twice here
        summary (stats/summary weights :trim? true)
        normal-sample (random/sample (count weights)
                                 #(random/normalvariate (:mean summary) (:stddev summary)))
        xy (map vector
                (stats/trim normal-sample)
                (stats/trim weights))

        logweights (map #(Math/log %) weights)
        summary (stats/summary logweights :trim? true)
        normal-sample (random/sample (count logweights)
                                 #(random/normalvariate (:mean summary) (:stddev summary)))
        xlogy (map vector
                   (stats/trim normal-sample)
                   (stats/trim logweights))]
        (util/write-to-csv "tmp/weights.csv" (conj xy ["x" "y"]))
        (util/write-to-csv "tmp/weights-log.csv" (conj xlogy ["x" "y"]))))

(defn process-irs-csv
  "Pulled data file from http://www.irs.gov/uac/SOI-Tax-Stats---Individual-Statistical-Tables-by-Size-of-Adjusted-Gross-Income."
  [&{:keys [datafile] :or {datafile "data/10in11si.csv"}}]
  (letfn [(process-bracket [s]
            (let [tuple (map (comp util/str-to-float clojure.string/trim) 
                             (-> s
                                 (clojure.string/replace #"No adjusted gross income" "0")
                                 (clojure.string/replace #"\$|,| or more" "")
                                 (clojure.string/split #"under")))]
              (if (= (count tuple) 2)
                (/ (+ (first tuple) (second tuple)) 2)
                (/ (* 3 (first tuple) ) 2))))
          (process-values [s]
            (map (fn [x]
                   (-> x
                       (clojure.string/replace #"\[|\]|," "")
                       util/str-to-float ))
                 s))
          (make-hist [d]
            (into (sorted-map) (for [[b number] d] [b number])))]
    (let [data (csv/read-csv (io/reader datafile))
          data (drop-while #(not= (first %) "No adjusted gross income") data)
          data (take-while #(not= (first %) "Accumulated from Smallest Size of Adjusted Gross Income") data)
          data (map #(cons (process-bracket (first %))
                           (process-values (rest %)))
                    (map (partial take 3) data))
          h (make-hist data)
          mean (hist/hist->mean h)
          cdff (cdf/cdff h)
          median (cdff 0.5 :value)
          pmf (hist/hist->pmf h)
          stddev (hist/pmf->stddev pmf)
          total-reporting-taxes (reduce +  (map second  (seq  h)))
          pop-below-mean (reduce +
                                 (map second  (take-while
                                                #(<=  (first %) mean)
                                                (seq  h))))]
      ; TODO: compute the skew and the gini coeff
      {:raw data
       :hist h
       :cdff cdff
       :mean mean
       :median median
       :population-below-mean pop-below-mean
       :total total-reporting-taxes
       :fraction-below-mean (/ pop-below-mean total-reporting-taxes)})))
