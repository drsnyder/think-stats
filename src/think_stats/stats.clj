(ns think-stats.stats)

(defn mean
  [s]
  (assert (sequential? s) "Cannot compute the mean on a non-seq.")
  (if (empty? s)
    nil
    (/ (reduce + s) 
       (count s))))

(defn square [x] (* x x))

(defn variance
  [s &{:keys [sample] :or {sample false}}]
  (assert (sequential? s) "Cannot compute the variance on a non-seq.")
  (let [n (if sample 
            (- (count s) 1) 
            (count s))]
    (when-let [m (mean s)]
      (/ (reduce + (map #(square (- % m)) s))
         n))))

(defn stddev
  [s]
  (Math/sqrt (variance s)))


(defn hist
  [s]
  (assert (sequential? s) "Cannot compute the hist on a non-seq.")
  (frequencies s))


(defn pmf
  [s]
  (assert (sequential? s) "Cannot compute the pmf on a non-seq.")
  (let [n (count s)]
    (into {} (map 
               #(vector (first %) (/ (second %) n)) 
               (frequencies s)))))


(defn pmf->remaining-lifetime
  [pmf] 
  (reduce (fn [acc v]
            (assoc acc (first v)
                   (reduce + 
                           (map second 
                                (filter #(> (first %) (first v)) 
                                        pmf)))))
            {} pmf))
