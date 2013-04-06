(ns think-stats.homeless
  (:require (think-stats [stats :as stats]
                         [util :as util])))

(defn select*
  [s fields] 
   (for [r s] 
     (select-keys r fields)))


(defn map-map
  [f m]
  (into {} (for [[k v] m] [k (f v)])))


; ch 3
(def sizes
  {(range 5 10) 8
   (range 10 15) 8
   (range 15 20) 14
   (range 20 25) 4
   (range 25 30) 6
   (range 30 35) 12
   (range 35 40) 8
   (range 40 45) 3
   (range 45 50) 2})

(def total-classes (stats/sum (vals sizes)))

(def sizes-pmf (into {} (for [[k v] sizes] [(stats/mean k) (/ v total-classes)])))

(defn plot-class-sizes
  [sizes-pmf &{:keys [csv-out r-script to-plot] :or 
     {csv-out "plots/3-sizes.csv" r-script "plots/3-sizes.R" to-plot "plots/3-sizes.png"}}]
  (let [sampled (stats/pmf->key-ordered (map-map float sizes-pmf))
        unbiased (stats/pmf->key-ordered (stats/bias-pmf sizes-pmf :invert true))
        combined (concat (list (list "size" "sampled" "unbiased"))
                         (for [k (keys sampled)]
                           (list k (sampled k) (unbiased k))))]
    (util/write-to-csv csv-out combined)
    (util/shell-exec (format "Rscript %s %s %s" r-script csv-out to-plot))))

    
        

