(ns think-stats.plots
  (:require (think-stats
              [util :as util]
              [stats :as stats]
              [distributions :as d]
              [homeless :as h])))

(defn f->line
  "Handy for plotting the CDF and CCDF. Under the hood it uses a simple R line plotting script.

  Example using the exponential distribution using lambda = 1:

  (def e (take 1000 (repeatedly #(d/expovariate 1))))
  (def cdf (d/cdff e :to-float true))
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


