(ns think-stats.homeless
  (:require (think-stats [stats :as stats]
                         [util :as util])))


(defn map-map
  [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn- vswap! 
  [v a b]
  (let [t (nth v b)]
    (assoc! v b (nth v a))
    (assoc! v a t)))

(defn- partition-by-idx [l left right idx]
  (let [pivot-value (nth l idx)
        store-index (atom left)]
    (vswap! l idx right)
    (loop [i left]
      (when (< (nth l i) pivot-value)
        (vswap! l @store-index i)
        (swap! store-index inc))
      (when (< i right)
        (recur (inc i))))
    (vswap! l right @store-index) 
    @store-index))

; see http://en.wikipedia.org/wiki/Selection_algorithm
(defn select 
  [l left right k]
  (if (= left right)
    (nth l left)
    ; this is an arbitrary selection of the pivot index that may not serve use
    ; well in some cases
    (let [new-index (partition-by-idx l left right (int (/ (+ left right) 2)))
          dist (inc (- new-index left))]
      (cond 
        (= dist k) (nth l new-index)
        (= new-index k) (nth l new-index) ; same as above
        (< k dist) (select l left (dec new-index) k)
        :else (select l (inc new-index) right (- k dist))))))

(defmulti compute-bisection-ends (fn [dir s x low mid high] dir))

(defmethod compute-bisection-ends :right 
  ([dir s x low mid high]
   (cond
     (< x (nth s mid)) [low mid]
     :else [(inc mid) high])))

(defmethod compute-bisection-ends :left
  ([dir s x low mid high]
   (cond
     (< (nth s mid) x) [(inc mid) high]
     :else [low mid])))
  
(defmethod compute-bisection-ends :default
  ([dir s x low mid high]
   (compute-bisection-ends :right s x low mid high)))

(defn bisect
  [s x & [dir]] 
  (loop [low 0 
         high (count s)]
    (if (>= low high)
      low
      (let [mid (quot (+ low high) 2)
            [l h] (compute-bisection-ends dir s x low mid high)]
        (prn (>= high 10) low mid high x)
        (recur l h)))))
    


;;;;;;;;;;;;;
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
        unbiased (stats/pmf->key-ordered (stats/pmf->unbiased sizes-pmf))
        combined (concat (list (list "size" "sampled" "unbiased"))
                         (for [k (keys sampled)]
                           (list k (sampled k) (unbiased k))))]
    (util/write-to-csv csv-out combined)
    (util/shell-exec (format "Rscript %s %s %s" r-script csv-out to-plot))))


(defn plot-speeds
  ([speed &{:keys [csv-out r-script to-plot] 
            :or {csv-out "plots/3-speeds.csv" r-script "plots/3-speeds.R" 
                 to-plot "plots/3-speeds.png"}}]
   (let [speeds (map #(Float/parseFloat %) 
                     (clojure.string/split (slurp "data/speeds.txt") #"\n"))
         speeds-pmf (map-map float (stats/pmf speeds))
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
     (prn speeds-pmf)
     (util/write-to-csv csv-out speeds-csv)
     (util/shell-exec (format "Rscript %s %s %s" r-script csv-out to-plot))))
  ([] (plot-speeds 7.5)))


    
        

