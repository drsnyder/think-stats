(ns think-stats.homeless)

(defn select*
  [s fields] 
   (for [r s] 
     (select-keys r fields)))



