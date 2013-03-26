(ns think-stats.pregnancy
  (:require (think-stats
              [util :as util]
              [survey :as s])))

(def fields [(s/def-field-extractor "caseid" 0 12)
             (s/def-field-extractor "nbrnaliv" 21 22)
             (s/def-field-extractor "babysex" 55 56)
             (s/def-field-extractor "birthwgt_lb" 56 58)
             (s/def-field-extractor "birthwgt_oz" 58 60)
             (s/def-field-extractor "prglength" 274 276)
             (s/def-field-extractor "outcome" 276 277)
             (s/def-field-extractor "birthord" 277 279)
             (s/def-field-extractor "agepreg" 284 287)
             (s/def-field-extractor "finalwgt" 422 440 util/str-to-float)])
