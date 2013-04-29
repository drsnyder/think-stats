(ns think-stats.chapters.four
  (:require (think-stats [stats :as stats]
                         [util :as util]
                         [survey :as s]
                         [homeless :as h]
                         [distributions :as d])))

; TODO: confirm that this has the slope and intesect that we expect
(defn pareto-cdf
  "4.3 Plots plots the pareto CDF and CCDF. The CCDF is plotted on a log-log scale. See also plots/pareto.R.
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
    (util/shell-exec (format "Rscript %s %s" r-script csv-out))))

