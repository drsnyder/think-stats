(ns think-stats.chapters.three
  (:require (think-stats [stats :as stats]
                         [util :as util]
                         [survey :as s]
                         [homeless :as h]
                         [distributions :as d])))


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
  (let [sampled (stats/pmf->key-ordered (h/map-map float sizes-pmf))
        unbiased (stats/pmf->key-ordered (stats/pmf->unbiased sizes-pmf))
        combined (concat (list (list "size" "sampled" "unbiased"))
                         (for [k (keys sampled)]
                           (list k (sampled k) (unbiased k))))]
    (util/write-to-csv csv-out combined)
    (util/shell-exec (format "Rscript %s %s %s" r-script csv-out to-plot))))


(defn plot-speeds
  "Plots the speeds for a race relative to the given speed or as the runner running at the given speed would see them."
  ([speed &{:keys [csv-out r-script to-plot] 
            :or {csv-out "plots/3-speeds.csv" r-script "plots/3-speeds.R" 
                 to-plot "plots/3-speeds.png"}}]
   (let [speeds (map #(Float/parseFloat %) 
                     (clojure.string/split (slurp "data/speeds.txt") #"\n"))
         speeds-pmf (h/map-map float (stats/pmf speeds))
         speeds->unbias-by-runner (fn [pmf speed]
                                    (stats/normalize-pmf
                                      (into {} (for [[k v] pmf] 
                                                 [k (* v
                                                       (Math/abs (- v speed)))]))))
         speeds-biased (speeds->unbias-by-runner speeds-pmf speed)
         speeds-csv (concat (list (list "speed" "sampled" "biased")) 
                            (for [k (keys speeds-pmf)] (list k 
                                                             (speeds-pmf k) 
                                                             (speeds-biased k))))]
     (util/write-to-csv csv-out speeds-csv)
     (util/shell-exec (format "Rscript %s %s %s" r-script csv-out to-plot))))
  ([] (plot-speeds 7.5)))


(defn plot-speeds-cdf
  "Plots the CDF of runner speeds"
  ([&{:keys [csv-out r-script to-plot] 
      :or {csv-out "plots/3-speeds-cdf.csv" r-script "plots/3-speeds-cdf.R" 
           to-plot "plots/3-speeds-cdf.png"}}]
   (let [speeds (map #(Float/parseFloat %) 
                     (clojure.string/split (slurp "data/speeds.txt") #"\n"))
         speeds-cdf (h/map-map float (d/cdf speeds) :dest (sorted-map))
         speeds-csv (concat (list (list "speed" "cdf(x)")) 
                            (for [k (keys speeds-cdf)] (list k 
                                                             (speeds-cdf k))))]
     (util/write-to-csv csv-out speeds-csv)
     (util/shell-exec (format "Rscript %s %s %s" r-script csv-out to-plot)))))

(declare birth-weight-data)
(defn birth-weight-cdfs
  [data-file &{:keys [csv-out r-script to-plot] 
      :or {csv-out "plots/3-birthweights-cdf.csv" r-script "plots/3-birthweights-cdf.R" 
           to-plot "plots/3-birthweights-cdf.png"}}]
  (let [totalwgts (birth-weight-data data-file)
         cdf-data (d/cdf totalwgts)
         cdf (d/cdff totalwgts)
         cdf-sample-data (d/cdf (d/sample cdf 1000))
         csv (concat (list (list "weight" "survey" "sample"))
                     (for [k (sort (keys cdf-data)) :when (and 
                                                            (get cdf-data k) 
                                                            (get cdf-sample-data k))]
                       (list k (float (get cdf-data k)) 
                             (float (get cdf-sample-data k)))))]
     (util/write-to-csv csv-out csv)
     (util/shell-exec (format "Rscript %s %s %s" r-script csv-out to-plot))))

(defn birth-weight-data
  [data-file]
  (let [preg-data (util/read-file data-file :gunzip true)
        db (map (partial s/line->fields s/fields) preg-data)
        predicate (fn [r] 
                    (when-let [len (get r "prglength")]
                      (and 
                        (get r "birthwgt_lb" nil)
                        (get r "birthwgt_oz" nil)
                        (get r "birthord" nil)
                        (= (get r "outcome") 1)))) ; only live births
        all (for [r db :when (predicate r)]
              (if (and (not= (get r "birthwgt_lb") "NA")
                       (< (get r "birthwgt_lb") 20)
                       (not= (get r "birthwgt_oz") "NA")
                       (<= (get r "birthwgt_oz") 16))
                (assoc r "totalwgt_oz" (+ (* (get r "birthwgt_lb") 16) 
                                          (get r "birthwgt_oz")))
                (assoc r "totalwgt_oz" "NA")))
        totalwgts (for [r all :when (not= (get r "totalwgt_oz") "NA")] 
                    (get r "totalwgt_oz"))]
    totalwgts))





























