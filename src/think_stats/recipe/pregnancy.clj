(ns think-stats.recipe.pregnancy
  (:require (think-stats [util :as util]
                         [homeless :as h]
                         [survey :as s]
                         [pregnancy :as preg]
                         [stats :as stats])))

; FIXME: move think-stats.pregnancy here

(defn plot-length-hist
  [data-file csv-out r-script]
  (let [preg-data (util/read-file data-file :gunzip true)
        db (map (partial s/line->fields preg/fields) preg-data)
        prg-len-by-ord (concat 
                         ; header
                         (list (list "birthord" "prglength")) 
                         ; row tuples with nil's removed
                         (for [r db :when (not= (get r "birthord") nil)] 
                           (list 
                             (if (= (get r "birthord") 1) "first babies" "others") 
                             (get r "prglength"))))]
    (util/write-to-csv csv-out prg-len-by-ord)
    (util/shell-exec (format "Rscript %s %s" r-script csv-out))))






