(ns think-stats.hist)

(defprotocol HistProtocol
  (freq [this])
  (items [this])
  (->map [this])
  (->seq [this])
  (pmf [this])
  (cdf [this]))


(defn build-hist
  "Build a histogram from a sequence."
  [s]
  (assert (sequential? s) "Cannot compute the hist on a non-seq.")
  (into (sorted-map)
        (for [[k v] (frequencies s)] 
          [k v])))

(defn hist->pmf
  "Convert a histogram into a PMF."
  [h &{:keys [to-float] :or {to-float false}}]
  (let [total (reduce + (for [[k v] (seq h)] 
                          v))]
    (into (sorted-map) (for [[k v] (seq h)
                             :let [m (/ v total)
                                   m (if to-float (float m) m)]]
                         [k m]))))

(defn hist->cdf
  "Assumes that h is sorted. Another reason to create a type?"
  [h &{:keys [to-float] :or {to-float false}}]
  (let [total (reduce + (freq h))]
    (into (sorted-map)
         (for [[i acc] (map vector (items h) (reductions + (freq h)))
               :let [y (/ acc total)
                     y (if to-float (float y) y)]]
           [i y]))))



(defrecord Hist [h]
  HistProtocol
  (freq [this]
    (vals h))
  (items [this]
    (keys h))
  (->map [this]
    h)
  (->seq [this]
    (seq h))
  (pmf [this]
    (hist->pmf h))
  (cdf [this]
    (hist->cdf h)))


(defn seq->hist
  "Create a Hist record from a seq."
  [s]
  (->Hist (build-hist s)))
