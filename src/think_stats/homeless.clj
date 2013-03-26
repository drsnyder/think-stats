(ns think-stats.homeless)

(defn select*
  [s field]
  (for [r s] (get r field)))


