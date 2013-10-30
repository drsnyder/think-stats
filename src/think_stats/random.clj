(ns think-stats.random
  (:require (think-stats
              [distributions :as d]
              [constants :as c]
              [homeless :as h]
              [stats :as stats]
              [cdf :as cdf]
              [util :as util]
              [chi-squared :as chi-squared]))
  (:import org.apache.commons.math3.special.Erf
           org.apache.commons.math3.distribution.BinomialDistribution
           org.apache.commons.math3.distribution.GammaDistribution))

(declare normalvariate)

(def standard-normal-mu 0)    ; mean
(def standard-normal-sigma 1) ; standard deviation
(def standard-normalvariate #(normalvariate standard-normal-mu standard-normal-sigma))

(def rankit-items 6)

(defn sample-seq
  "Randomly select n items from s without replacement."
  [n s]
  (assert (sequential? s) "Error, cannot sample without replacement a non-seq.")
  (take n (shuffle s)))

(defn choice-seq
  "Randomly select n items from s with replacement."
  [n s]
  (assert (sequential? s) "Error, cannot sample with replacement a non-seq.")
  (repeatedly n #(rand-nth s)))

(defn sample
  "Sample f by calling it n times."
  [n f]
  (repeatedly n f))

(defn sum-samples
  "Compute Z = X + Y given two functions X, and Y that generate random numbers."
  [n X Y]
  (repeatedly n #(+ (X) (Y))))

(defn rankit-sample
  ([X]
   (into [] (sample rankit-items X)))
  ([]
   (rankit-sample standard-normalvariate)))

(defn rankit-samples
  ([X n]
   (map stats/mean
        (apply map vector
               (map sort
                    (repeatedly n #(rankit-sample X))))))
  ([n]
   (rankit-samples standard-normalvariate n)))



(defn normalvariate
  "Generate random values from a normal distribution with mean mu and standard deviation sigma."
  [mu sigma]
  (cdf/normalicdf mu sigma (- 1.0 (rand))))


(defn lognormalvariate
  "Log normal distribution."
  [mu sigma]
  (Math/exp (normalvariate mu sigma)))

(defn lognormalmean
  [mu sigma]
  (let [var (h/square sigma)]
    (Math/exp (+ mu (/ var 2)))))

(defn lognormalvariance
  [mu sigma]
  (let [var (h/square sigma)]
    (*
      (- (Math/exp var) 1)
      (Math/exp (+ (* 2 mu) var)))))


(defn expovariate
  "Generate random values from an exponential distribution with rate parameter lambda.
  See http://en.wikipedia.org/wiki/Exponential_distribution generating exponential variates"
  [lambda]
  (* -1.0
     (/ (Math/log (- 1.0 (rand)))
        (float lambda))))

(defn expomedian
  "Compute the median of an exponential distribution with a given rate parameter lambda."
  [lambda]
  (/ (Math/log 2) lambda))


(defn expomean
  "Compute the mean of an exponential distribution with a given rate parameter lambda."
  [lambda]
  (/ 1.0 lambda))

(defn expopdf
  "Compute the PDF of the exponential distrubution with lambda at x."
  [lambda x]
  (* lambda (Math/exp (* (* -1 lambda) x))))

(defn expo-interval
  [sample alpha]
  (let [n (count sample)
        n2 (* 2 n)
        estimator (/ 1.0 (stats/mean sample n))]
    [(* estimator (/ (chi-squared/cdf n2 (- 1 (/ alpha 2.0))) n2))
     (* estimator (/ (chi-squared/cdf n2 (/ alpha 2.0)) n2))]))

(defn erlangvariate
  "Generate random values from an Erlang distribution with rate parameter lambda
  and shape parameter k."
  [lambda k]
  (reduce + (repeatedly k #(expovariate lambda))))


(defn create-gamma
  "Create a gamma distribution object. Uses org.apache.commons.math3.distribution.GammaDistribution for generating
  random variates."
  [shape scale]
  (GammaDistribution. shape scale))

(def gamma (memoize create-gamma))

(defn sample-gamma
  [g]
  (.sample g))

(defn gammavariate
  "Generate random values from a gamma distribution with the given shape and scale."
  [shape scale]
  (sample-gamma (gamma shape scale)))

(defn create-binomial
  [trials p]
   (BinomialDistribution. trials p))

(def binomial (memoize create-binomial))

(defn binomialvariate
  "Generate random values from a binomial distribution with the given trials and
  probability p."
  [trials p]
  (.inverseCumulativeProbability (binomial trials p) (- 1.0 (rand))))

(defn weibullvariate
  "See http://en.wikipedia.org/wiki/Weibull_distribution.

  A transformation that should produce a straight line (4.5):
  log(log(1/(1-y))) = beta*log(x) - beta*log(lambda)
  y = b + ax
  intercept = beta * log(lambda)
  slope = beta*log(x)

  Example:
  (def w (repeatedly 100000 #(random/weibullvariate 1 0.5)))
  (def cdf (cdf/cdf w))
  ; < 2.5 to match the wikipedia page for comparison
  ; FIXME: compute the CCDF
  (def cdf (into {} (for [k (filter #(< % 2.5) (keys cdf))] [k (get cdf k))))
  (plots/line (keys cdf) (vals cdf))
  "
  [lambda beta]
  (* lambda (Math/pow (* -1.0 (Math/log (- 1.0 (rand))))
                      (/ 1.0 beta))))

(defn weibull-line
  [s lambda beta]
  (let [cdf (cdf/cdf s)
        x (keys cdf)
        y (vals cdf)]
    [(map #(- (* beta (Math/log %)) (* beta (Math/log lambda))))
     (map #(Math/log (Math/log (/ (- 1 %)) )) y)]))

(defn paretovariate
  "See http://en.wikipedia.org/wiki/Pareto_distribution for random sample generation."
  ([alpha x-min]
   (* x-min
     (/ 1.0 (Math/pow (- 1.0 (rand))
                      (/ 1 alpha)))))
  ([alpha]
   (paretovariate alpha 1)))

(defn paretomedian
  "Compute the median of a Pareto distribution with the given alpha and threshold."
  ([alpha x-min]
   (* x-min (Math/pow 2 (/ 1 alpha))))
  ([alpha]
   (paretomedian alpha 1)))

(defn paretomean
  "Compute the mean of a Pareto distribution with the given alpha and threshold."
  ([alpha x-min]
   (assert (> alpha 1))
   (/ (* alpha x-min)
      (- alpha 1)))
  ([alpha]
   (paretomean alpha 1)))

(defn gumbelvariate
  [mu beta]
  (+ mu
     (* beta
        (Math/log (/ 1.0
                     (Math/log (/ 1.0
                                  (rand))))))))

(defn fair-dice
  "Simulate a fair dice. Defaults to 6 sides."
  ([n]
   (inc (rand-int n)))
  ([]
   (fair-dice 6)))
