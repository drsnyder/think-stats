(ns think-stats.correlation
  (:require (think-stats
              [stats :as stats]
              [homeless :as h])))

(defn- summary-stats
  [X Y]
  (let [Xn (count X)
        Yn (count Y)
        _ (assert (= Xn Yn))
        Xu (stats/mean X Xn)
        Yu (stats/mean Y Yn)]
    {:count-x Xn
     :count-y Yn
     :mean-x Xu
     :mean-y Yu}))

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
   (cov X Y (summary-stats X Y))))

(defn pearsons-correlation
  "Compute the Pearson's correlation coefficient for X and Y. X and Y must be of the same length."
  [X Y]
  (let [{Xn :count-x Yn :count-y Xu :mean-x Yu :mean-y :as summary} (summary-stats X Y)
        cov (cov X Y summary)
        Xsd (Math/sqrt (stats/mean-variance X h/square Xn Xu))
        Ysd (Math/sqrt (stats/mean-variance Y h/square Yn Yu))]
    (/ cov
       (* Xsd Ysd))))


(defn sum-squares
  [X Xu]
  (h/sum (stats/mean-deviations X Xu h/square)))

(defn sum-deviation-products
  [X Xu Y Yu]
  (h/sum (map #(apply * %)
              (map vector
                   (stats/mean-deviations X Xu)
                   (stats/mean-deviations Y Yu)))))

(defn raw-r
  "This method appears to be the fastest in their current forms."
  [X Y]
  (let [{Xn :count-x Yn :count-y Xu :mean-x Yu :mean-y :as summary} (summary-stats X Y)]
    (/ (sum-deviation-products X Xu Y Yu)
       (Math/sqrt (* (sum-squares X Xu)
                     (sum-squares Y Yu))))))

(defn z-r
  [X Y]
  (let [{Xn :count-x Yn :count-y Xu :mean-x Yu :mean-y :as summary} (summary-stats X Y)
        Xsd (stats/stddev X Xn)
        Ysd (stats/stddev Y Yn)]
    (/ (h/sum (map #(apply * %)
                   (map vector
                        (map #(stats/z % Xu Xsd) X)
                        (map #(stats/z % Yu Ysd) Y))))
       Xn)))
