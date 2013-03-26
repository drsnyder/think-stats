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

