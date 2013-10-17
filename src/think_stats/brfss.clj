(ns think-stats.brfss
  (:require (think-stats
              [util :as util]
              [stats :as stats]
              [survey :as s])))

(def fields [(s/def-field-extractor "age" 100 102)
             (s/def-field-extractor "weight2" 118 122)
             (s/def-field-extractor "wtyrago" 126 130)
             (s/def-field-extractor "wtkg2" 1253 1258)
             (s/def-field-extractor "htm3" 1250 1253)
             (s/def-field-extractor "sex" 142 143)])

(declare load-data)


(defn load-data
  [data-file]
  (let [data (util/read-file data-file :gunzip true)
        db (map (partial s/line->fields fields) data)
        weight-check (fn [r]
                    (when-let [weight (get r "weight2" nil)]
                      (and (number? weight)
                           (not= weight 7777)
                           (not= weight 9999))))
        height-check (fn [r]
                       (when-let [height (get r "htm3")]
                         (and (number? height)
                              (not= height 999))))
        clean (fn [r field]
                (let [v (get r field nil)]
                  (if (number? v)
                    (cond-> r
                            (< v 1000) (update-in [field] #(/ % 2.2))
                            (and (> v 9000) (< v 9999)) (update-in [field] #(- % 9000)))
                    r)))
        all (for [r db :when (and (not (nil? r)) (weight-check r) (height-check r))]
              (-> r (clean "weight2") (clean "wtyrago")))]
    all))
