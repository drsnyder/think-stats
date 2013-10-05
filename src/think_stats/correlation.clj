(ns think-stats.correlation
  (:require (think-stats
              [stats :as stats]
              [homeless :as h])))

(defn cov-summary-stats
  [X Y]
  (let [Xn (count X)
        Yn (count Y)]
    (assert (= Xn Yn))
    (let [Xu (stats/mean X Xn)
          Yu (stats/mean Y Yn)]
      {:count-x Xn
       :count-y Yn
       :mean-x Xu
       :mean-y Yu})))

(defn cov
  "Compute the covariance of X and Y. X and Y must be of the same length."
  ([X Y summary]
   (assert (and (sequential? X) (sequential? Y)))
   (let [{Xn :count-x Yn :count-y Xu :mean-x Yu :mean-y} summary
         delta (fn [mean i] (- i mean))]
     (/ (h/sum (for [pair (map list
                               (map (partial delta Xu) X)
                               (map (partial delta Yu) Y))]
                 (apply * pair)))
        Xn)))
  ([X Y]
   (cov X Y (cov-summary-stats X Y))))

(defn pearsons-correlation
  [X Y]
  (let [{Xn :count-x Yn :count-y Xu :mean-x Yu :mean-y :as summary} (cov-summary-stats X Y)
        cov (cov X Y summary)
        Xsd (Math/sqrt (stats/mean-variance X h/square Xn Xu))
        Ysd (Math/sqrt (stats/mean-variance Y h/square Yn Yu))]
    (/ cov
       (* Xsd Ysd))))

