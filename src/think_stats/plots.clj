(ns think-stats.plots
  (:require (think-stats
              [util :as util]
              [stats :as stats]
              [distributions :as d]
              [homeless :as h])))

(defn cdf
  "(plots/cdf (partial d/exponential 2) (range 0 2.5 0.01))"
  [cdf x-vals &{:keys [r-script csv-out to-plot title] 
                :or {r-script "plots/cdf.R" csv-out "plots/cdf.csv" to-plot "plots/cdf.png" title "CDF(x)"}}]
  (let [csv (concat (list (list "x" "y"))
                    (for [x x-vals] (list x (cdf x))))]
    (util/write-to-csv csv-out csv)
    (util/shell-exec (format "Rscript %s %s %s %s" r-script csv-out title to-plot))))


