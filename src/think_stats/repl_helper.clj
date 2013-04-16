(ns think-stats.repl-helper
  (:require (think-stats [util :as util]
                         [homeless :as h]
                         [survey :as s]
                         [distributions :as d]
                         [pregnancy :as preg]
                         [stats :as stats])
            (think-stats.chapters [three :as three])
            [midje.repl :refer :all]))

