(ns think-stats.pregnancy
  (:require (think-stats
              [util :as util]
              [survey :as s])))

(def fields [(s/def-field-extractor "caseid" 1 2)
             (s/def-field-extractor "nbrnaliv" 22 22)
             (s/def-field-extractor "babysex" 56 56)
             (s/def-field-extractor "birthwgt_lb" 57 58)
             (s/def-field-extractor "birthwgt_oz" 59 60)
             (s/def-field-extractor "prglength" 275 276)
             (s/def-field-extractor "outcome" 277 277)
             (s/def-field-extractor "birthord" 278 279)
             (s/def-field-extractor "agepreg" 284 287)
             (s/def-field-extractor "finalwgt" 423 440 util/str-to-float)])
