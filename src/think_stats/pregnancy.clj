(ns think-stats.pregnancy
  (:require (think-stats
              [util :as util])))

(def fields [["caseid" 1 12 util/str-to-int]
             ["nbrnaliv" 22 22 util/str-to-int]
             ["babysex" 56 56 util/str-to-int]
             ["birthwgt_lb" 57 58 util/str-to-int]
             ["birthwgt_oz" 59 60 util/str-to-int]
             ["prglength" 275 276 util/str-to-int]
             ["outcome" 277 277 util/str-to-int]
             ["birthord" 278 279 util/str-to-int]
             ["agepreg" 284 287 util/str-to-int]
             ["finalwgt" 423 440 util/str-to-float]])
