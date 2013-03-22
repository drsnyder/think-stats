(ns think-stats.stats)

(defn mean
  [s]
  (assert (sequential? s) "Cannot compute the mean on a non-seq.")
  (if (empty? s)
    nil
    (/ (reduce + s) 
       (count s))))
