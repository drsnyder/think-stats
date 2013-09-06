(ns user
  (:use [clojure.repl :only (doc)])
  (:require (think-stats
              [constants :as c]
              [util :as util]
              [homeless :as h]
              [survey :as s]
              [distributions :as d]
              [random :as random]
              [probability :as p]
              [pregnancy :as preg]
              [brfss :as brfss]
              [stats :as stats]
              [hist :as hist]
              [cdf :as cdf]
              [plots :as plots]
              [chi-squared :as chi-squared])
            (think-stats.chapters [three :as three]
                                  [four :as four]
                                  [five :as five]
                                  [six :as six]
                                  [seven :as seven])
            [clj-http.client :as client]
            [clojure.pprint :refer [pprint]]
            [midje.repl :refer :all]
            [clojure.tools.namespace.repl :refer  (refresh refresh-all)])
  (:import [think-stats.types]))
