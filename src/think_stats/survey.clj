(ns think-stats.survey
  (:require (think-stats
              [util :as util])))

(defn def-field-extractor
  ([field-name start end cast-f recode]
   [field-name start end cast-f recode])
  ([field-name start end cast-f]
   (def-field-extractor field-name start end cast-f identity))
  ([field-name start end]
   (def-field-extractor field-name start end util/str-to-int identity)))

(def fields [(def-field-extractor "caseid" 0 12)
             (def-field-extractor "nbrnaliv" 21 22)
             (def-field-extractor "babysex" 55 56)
             (def-field-extractor "birthwgt_lb" 56 58)
             (def-field-extractor "birthwgt_oz" 58 60)
             (def-field-extractor "prglength" 274 276)
             (def-field-extractor "outcome" 276 277)
             (def-field-extractor "birthord" 277 279)
             (def-field-extractor "agepreg" 284 287)
             (def-field-extractor "finalwgt" 422 440 util/str-to-float)])

(defn extract-field 
  [s off len]
  (clojure.string/trim (subs s off len)))



(defn field-name
  [d]
  (nth d 0))

(defn field-start 
  [d]
  (nth d 1))

(defn field-end
  [d]
  (nth d 2))

(defn field-type
  [d]
  (nth d 3))

(defn field-recode
  [d]
  (nth d 4 identity))

(defn field->column
  [v field-name to-type recode]
  {field-name (recode (to-type v))})

(defn line->fields
  [field-def line]
  (apply merge
        (map #(field->column 
                (extract-field line
                               (field-start %)
                               (field-end %))
                (field-name %) 
                (field-type %)
                (field-recode %))
                field-def)))

