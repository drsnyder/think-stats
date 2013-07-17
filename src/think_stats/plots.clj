(ns think-stats.plots
  (:require (think-stats
              [util :as util]
              [stats :as stats]
              [cdf :as cdf]
              [distributions :as d]
              [homeless :as h])))

(defn f->line
  "Handy for plotting the CDF and CCDF. Under the hood it uses a simple R line plotting script.

  Example using the exponential distribution using lambda = 1:

  (def e (take 1000 (repeatedly #(random/expovariate 1))))
  (def cdf (cdf/cdff e))
  (plots/line cdf (range 0 2.5 0.01)) ; exponential distribution 

  ; complement of the exponential distribution - straight line with slope -lambda
  (plots/line (comp #(Math/log (- 1 %)) cdf) (range 0 2.5 0.01))
  "
  [cdf x-vals &{:keys [r-script csv-out to-plot title] 
                :or {r-script "plots/line.R" csv-out "plots/line.csv" 
                     to-plot "plots/line.png" title "f(x)"}}]
  (let [header (list (list "x" "y"))
        data   (for [x x-vals :let [y (cdf x)]] (list x y))
        csv    (concat header data)]
    (util/write-to-csv csv-out csv)
    (util/shell-exec (format "Rscript %s %s %s %s" r-script csv-out title to-plot))))


(defn line
  "Line plot. Under the hood it uses a simple R line plotting script.

  Example:

  (def p (repeatedly 10000 (fn [] (random/paretovariate 1 0.5))))
  (def cdf (cdf/cdff p))
  (def xs (range 0 10 0.1))
  (def ys (map cdf xs))

  (line xs ys :to-plot \"plots/pareto-cdf.png\")
  (line (map #(Math/log %) xs) 
        (map #(Math/log (- 1 %)) ys) 
        :to-plot \"plots/pareto-ccdf.png\")

  "
  [x-vals y-vals &{:keys [r-script csv-out to-plot title] 
                   :or {r-script "plots/line.R" csv-out "plots/line.csv"
                        to-plot "plots/line.png" title "line"}}]
  (let [header ["x" "y"]
        data   (map vector x-vals y-vals)
        csv    (conj data header)]
    (util/write-to-csv csv-out csv)
    (util/shell-exec (format "Rscript %s %s %s %s" r-script csv-out title to-plot))))

