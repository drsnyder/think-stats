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
