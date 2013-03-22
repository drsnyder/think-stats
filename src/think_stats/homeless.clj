(ns think-stats.homeless)

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

(defn field->column
  [v field-name to-type]
  {field-name (to-type v)})

(defn line->fields
  [field-def line]
  (apply merge
        (map #(field->column 
                (extract-field line
                               (field-start %)
                               (field-end %))
                (field-name %) 
                (field-type %))
                field-def)))


