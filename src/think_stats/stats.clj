(ns think-stats.stats)

(defn sum
  [s]
  (reduce + s))

(defn mean
  [s]
  (assert (sequential? s) "Cannot compute the mean on a non-seq.")
  (if (empty? s)
    nil
    (/ (sum s) 
       (count s))))

(defn square [x] (* x x))

(defn variance
  [s &{:keys [sample] :or {sample false}}]
  (assert (sequential? s) "Cannot compute the variance on a non-seq.")
  (let [n (if sample 
            (- (count s) 1) 
            (count s))]
    (when-let [m (mean s)]
      (/ (sum (map #(square (- % m)) s))
         n))))

(defn stddev
  [s]
  (Math/sqrt (variance s)))


(defn hist
  [s]
  (assert (sequential? s) "Cannot compute the hist on a non-seq.")
  (frequencies s))


(defn trim
  "Trim's s by taking p % elements from both ends. The sequence is sorted before being trimmed."
  ([s p]
   (assert (sequential? s) "Cannot trim a non-seq.")
   (let [s (sort s)
         len (count s)
         n (int (* p len))]
     (drop n (take (- len n) s))))
  ([s]
   (trim s 0.01)))


(defn pmf
  [s]
  (assert (sequential? s) "Cannot compute the pmf on a non-seq.")
  (let [n (count s)]
    (into {} (map 
               #(vector (first %) (/ (second %) n)) 
               (frequencies s)))))

(defn pmf-entry->value
  [e]
  (first e))

(defn pmf-entry->freq
  [e]
  (second e))

(defn pmf->key-ordered
  [pmf &{:keys [dir] :or {dir :asc}}]
  (apply sorted-map-by (if (= dir :asc) < >) (flatten (seq pmf))))

(defn pmf->remaining-lifetime
  [pmf] 
  (loop [lifetimes (sort > (keys pmf))
         acc {}
         total 0]
    (if (not (empty? lifetimes))
      (let [k (first lifetimes)
            new-total (+ (pmf k) total)]
        (recur (rest lifetimes)
               (assoc acc k new-total)
               new-total))
      acc)))

(defn pmf->mean
  [pmf]
  (sum
    (map #(* (pmf-entry->value %) 
            (pmf-entry->freq %)) 
         (seq pmf))))

(defn pmf->variance
  [pmf]
  (let [m (pmf->mean pmf)]
    (sum 
      (map #(* (pmf-entry->freq %)
              (square (- (pmf-entry->value %) m))) 
           pmf))))


(defn bin-pmf-freq
  "Bin PMF values by applying binfn to each value in the PMF. 
  The resulting bins contain the sum of the frequencies that mapped to that bin."
  [pmf binfn]
  (sum (map pmf-entry->freq 
            (filter #(binfn (pmf-entry->value %)) 
                    (seq pmf)))))







