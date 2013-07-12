(ns think-stats.hist)

(defn seq->hist
  "Build a histogram from a sequence."
  [s]
  (assert (sequential? s) "Cannot compute the hist on a non-seq.")
  (into (sorted-map)
        (for [[k v] (frequencies s)]
          [k v])))

(declare hist->mass)
(declare hist->area)

(defn hist->pmf
  "Convert a histogram into a PMF."
  [h &{:keys [to-float] :or {to-float false}}]
  (assert (map? h) "Cannot compute the PMF of a histogram on a non-map.")
  (let [total (hist->mass h)]
    (into (sorted-map) (for [[k v] (seq h)
                             :let [m (/ v total)
                                   m (if to-float (float m) m)]]
                         [k m]))))

(defn hist->cdf
  "Assumes that h is sorted. Another reason to create a type?"
  [h &{:keys [to-float] :or {to-float false}}]
  (assert (map? h) "Cannot compute the CDF of a histogram on a non-map.")
  (let [total (hist->mass h)]
    (into (sorted-map)
         (for [[i acc] (map vector (keys h) (reductions + (vals h)))
               :let [y (/ acc total)
                     y (if to-float (float y) y)]]
           [i y]))))


(defn hist->mean
  "Compute the mean given a histogram."
  [h]
  (assert (map? h) "Cannot compute the mean of a histogram on a non-map.")
  (let [mass (hist->mass h)]
    (/ (hist->area h)
       mass)))

(defn hist->median
  "TODO: convert to CDF then fetch the value that corresponds to 0.5."
  [h]
  (assert (map? h) "Cannot compute the median of a histogram on a non-map."))

(defn hist->mass
  [h]
  (assert (map? h) "Cannot compute the total mass of a histogram on a non-map.")
  (reduce + (vals h)))

(defn hist->bin-widths
  "Compute the bin widths of the histogram."
  [h]
  (assert (map? h) "Cannot compute the bin widths a histogram on a non-map.")
  (map (partial apply -) (map reverse (partition 2 1 (keys h)))))

(defn hist->area
  "Compute the area of a histogram. Assumes the items represent with bin widths."
  [h]
  (assert (map? h) "Cannot compute the total area of a histogram on a non-map.")
  (assert (empty? (filter (complement number?) (keys h))) "Cannot compute the using non-numerical items.")
  (reduce + (map (partial apply *) (seq h))))
