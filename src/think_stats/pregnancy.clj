(ns think-stats.pregnancy
  (:require (think-stats
              [util :as util]
              [stats :as stats]
              [survey :as s])))

(def fields [(s/def-field-extractor "caseid" 0 12)
             (s/def-field-extractor "nbrnaliv" 21 22)
             (s/def-field-extractor "babysex" 55 56)
             (s/def-field-extractor "birthwgt_lb" 56 58)
             (s/def-field-extractor "birthwgt_oz" 58 60)
             (s/def-field-extractor "prglength" 274 276)
             (s/def-field-extractor "outcome" 276 277)
             (s/def-field-extractor "birthord" 277 279)
             (s/def-field-extractor "agepreg" 284 287)
             (s/def-field-extractor "finalwgt" 422 440 util/str-to-float)])


; FIXME: this was the most intuitive way to do this at first, but it's using
; ggplot2 to build the PMF. try building the pmf of each. try something like
; (sorted-map-by < (flatten (seq (stats/pmf ds))))
(defn plot-length-hist
  [data-file &{:keys [csv-out r-script to-plot] 
               :or {csv-out "plots/out.csv" r-script "plots/2.1.R" to-plot "plots/2.1.png"}}]
  (let [preg-data (util/read-file data-file :gunzip true)
        db (map (partial s/line->fields fields) preg-data)
        first-babies (stats/hist (for [r db :when (= (get r "birthord") 1)] (get r "prglength")))
        ; we should extract records where the prglength < 25
        other-babies (stats/hist (for [r db :when (not= (get r "birthord") 1)] (get r "prglength")))
        upper (+ (max (apply max (keys first-babies)) (apply max (keys other-babies))) 1)
        lower (min (apply min (keys first-babies)) (apply min (keys other-babies)))
        combined (concat (list (list "prglength" "first" "others")) 
                         (for [i (range lower upper)] (list i (get first-babies i 0) (get other-babies i 0))))]
    (util/write-to-csv csv-out combined)
    (util/shell-exec (format "Rscript %s %s %s" r-script csv-out to-plot))))

(defn plot-diff-hist
  [data-file &{:keys [csv-out r-script to-plot week-min week-max] 
                       :or {csv-out "plots/out.csv" r-script "plots/2.3.R" to-plot "plots/2.3.png" week-min 35 week-max 45}}]
  (let [preg-data (util/read-file data-file :gunzip true)
        db (map (partial s/line->fields fields) preg-data)
        predicate (fn [r] (and 
                            (>= (get r "prglength" 0) week-min)
                            (<= (get r "prglength" 99) week-max)))
        first-babies (stats/pmf (for [r db :when (and (= (get r "birthord") 1) (predicate r))] (get r "prglength")))
        other-babies (stats/pmf (for [r db :when (and (not= (get r "birthord") 1) (predicate r))] (get r "prglength")))
        upper (+ (max (apply max (keys first-babies)) (apply max (keys other-babies))) 1)
        lower (min (apply min (keys first-babies)) (apply min (keys other-babies)))
        combined (concat (list (list "prglength" "difference")) 
                         (for [i (range lower upper)] (list i (float 
                                                                (- 
                                                                  (get first-babies i 0) 
                                                                  (get other-babies i 0))))))]
    (prn combined)
    (util/write-to-csv csv-out combined)
    (util/shell-exec (format "Rscript %s %s %s" r-script csv-out to-plot))))

