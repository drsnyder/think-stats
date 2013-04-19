(ns think-stats.pregnancy
  (:require (think-stats
              [util :as util]
              [stats :as stats]
              [survey :as s])))



(declare load-data)

(defn plot-length-hist
  [data-file &{:keys [csv-out r-script to-plot week-min week-max week-min week-max] 
               :or {csv-out "plots/2.1.csv" r-script "plots/2.1.R" to-plot "plots/2.1.png" week-min 0 week-max 99} 
               :as params}]
  (let [[first-babies other-babies] (map stats/hist (apply load-data data-file params))
        upper (+ (max (apply max (keys first-babies)) (apply max (keys other-babies))) 1)
        lower (min (apply min (keys first-babies)) (apply min (keys other-babies)))
        combined (concat (list (list "prglength" "first" "others")) 
                         (for [i (range lower upper)] (list i (get first-babies i 0) (get other-babies i 0))))]
    (util/write-to-csv csv-out combined)
    (util/shell-exec (format "Rscript %s %s %s" r-script csv-out to-plot))))

(defn plot-diff-hist
  [data-file &{:keys [csv-out r-script to-plot week-min week-max] 
               :or {csv-out "plots/2.3.csv" r-script "plots/2.3.R" to-plot "plots/2.3.png" week-min 0 week-max 99} 
               :as params}]
  (let [[first-babies other-babies] (map stats/pmf (apply load-data data-file params))
        upper (+ (max (apply max (keys first-babies)) (apply max (keys other-babies))) 1)
        lower (min (apply min (keys first-babies)) (apply min (keys other-babies)))
        combined (concat (list (list "prglength" "difference")) 
                         (for [i (range lower upper)] (list i (float 
                                                                (* 100
                                                                  (- 
                                                                    (get first-babies i 0) 
                                                                    (get other-babies i 0)))))))]
    (util/write-to-csv csv-out combined)
    (util/shell-exec (format "Rscript %s %s %s" r-script csv-out to-plot))))



(defn on-time
  [pmf &{:keys [binfn] :or {binfn #{38 39 40}}}]
  (float (stats/bin-pmf-freq pmf binfn)))

(defn early
  [pmf &{:keys [binfn] :or {binfn (set (range 0 38))}}]
  (float (stats/bin-pmf-freq pmf binfn)))

(defn late
  [pmf &{:keys [binfn] :or {binfn (set (range 41 51))}}]
  (float (stats/bin-pmf-freq pmf binfn)))


(defn risk
  [data-file & params]
  (let [[first-babies other-babies live] (map (comp stats/pmf stats/trim) (apply load-data data-file params))]
    {:first
     {:on-time (on-time first-babies)
      :early (early first-babies)
      :late (late first-babies)}
     :other
     {:on-time (on-time other-babies)
      :early (early other-babies)
      :late (late other-babies)}
     :live
     {:on-time (on-time live)
      :early (early live)
      :late (late live)}}))

(defn plot-prob-week-x
  [data-file &{:keys [csv-out r-script to-plot week-min week-max] 
               :or {csv-out "plots/2.7.csv" r-script "plots/2.7.R" to-plot "plots/2.7.png" week-min 27 week-max 44} 
               :as params}]
  (let [[first-babies other-babies live] (map (comp stats/pmf stats/trim) (apply load-data data-file params))
        week-max (+ week-max 1)
        generator (fn [data-set x] (get (stats/normalize-pmf (stats/filter-map data-set (set (range x week-max)))) x 0))
        x (prn "inside " first-babies)
        x (prn "gen " (generator first-babies 0))
        rows (for [x (range week-min week-max)]
               [x
                (generator first-babies x)
                (generator other-babies x)
                (generator live x)])
        header (list "prglength" "first" "other" "live")
        combined (concat (list header) rows)]
    (util/write-to-csv csv-out combined)
    (util/shell-exec (format "Rscript %s %s %s" r-script csv-out to-plot))))




(defn load-data
  [data-file &{:keys [week-min week-max] :or {week-min 0 week-max 99} :as params}]
  (let [preg-data (util/read-file data-file :gunzip true)
        db (map (partial s/line->fields s/fields) preg-data)
        predicate (fn [r] 
                    (when-let [len (get r "prglength")]
                      (and 
                        (get r "birthord" nil)
                        (= (get r "outcome") 1) ; only live births
                        (>= len week-min)
                        (<= len week-max))))
        first-babies (for [r db :when (and (predicate r) (= (get r "birthord") 1))] (get r "prglength"))
        other-babies (for [r db :when (and (predicate r) (not= (get r "birthord") 1))] (get r "prglength"))
        live-births  (for [r db :when (and (predicate r) (= (get r "outcome") 1))] (get r "prglength"))]
    [first-babies other-babies live-births]))














