(ns think-stats.survey-test
  (:use [midje.sweet])
  (:require (think-stats [survey :as s]
                         [util :as u])))

(def test-survey-line "   1   100   25.0    fact")


(facts :survey :extract-field
       (s/extract-field test-survey-line 0 4) => "1"
       (s/extract-field test-survey-line 5 10) => "100"
       (s/extract-field test-survey-line 11 20) => "25.0"
       (s/extract-field test-survey-line 21 25) => "fact")

(facts :survey :def-field-extractor
       (s/def-field-extractor "foo" 1 2) => truthy
       (s/def-field-extractor "foo" 1 2) => ["foo" 1 2 u/str-to-int identity])

(facts :survey :lines->fields
       (let [definition [(s/def-field-extractor "id" 0 4)
                         (s/def-field-extractor "count" 5 10 u/str-to-int #(/ % 10))
                         (s/def-field-extractor "value" 11 20 u/str-to-float #(/ % 5))
                         (s/def-field-extractor "tag" 21 25 identity)]
             rec (s/line->fields definition test-survey-line)]
         (get rec "id") => 1
         (get rec "count") => 10
         (get rec "value") => 5.0
         (get rec "tag") => "fact"))



