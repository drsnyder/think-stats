(ns think-stats.repl-helper
  (:use '[clojure.repl :only (doc)])
  (:require (think-stats [util :as util]
                         [homeless :as h]
                         [survey :as s]
                         [distributions :as d]
                         [pregnancy :as preg]
                         [stats :as stats]
                         [plots :as plots])
            (think-stats.chapters [three :as three]
                                  [four :as four])
            [clj-http.client :as client]
            [midje.repl :refer :all]))

