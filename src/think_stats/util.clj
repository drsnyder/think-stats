(ns think-stats.util
  (:require [clojure.java.io  :as io]
            [clojure.java.shell :as shell]
            [clojure.data.csv :as csv]))

(defn str-to-int
  [s]
  (when (not (empty? s))
    (Integer/parseInt s)))

(defn str-to-float
  [s]
  (when (not (empty? s))
    (Float/parseFloat s)))


(defn split-cmd 
    "Split a command string (e.g. \"Rscript plots/2.1.R in-data.csv\") into it's constituent parts for input to sh."
    [cmd]
    (if (string? cmd)
          (clojure.string/split cmd #"\s+")
          cmd))

(defn shell-exec
    "Exec the given command on the shell."
    [cmd]
    (apply shell/sh (split-cmd cmd)))

(defn lazy-reader
  "Lazily read from fd."
  [fd]
  (lazy-seq
    (if-let [line (.readLine fd)]
      (cons line (lazy-reader fd))
      (.close fd))))

(defn read-file
  [file &{:keys [gunzip] :or {gunzip false}}]
  (lazy-reader (cond-> (io/input-stream file)
                         gunzip (java.util.zip.GZIPInputStream.)
                         true (io/reader))))

(defn write-to-csv 
  "Write the simulation data to the given file."
  [file data & opts]
  (let [writer (if opts (apply io/writer file opts) (io/writer file))]
  (with-open [out-file writer] 
    (csv/write-csv out-file data))))

(defn factorial
  [n]
  (letfn [(fact [i acc]
            (if (= i 0)
              acc
              (fact (dec i) (* i acc))))]
    (fact n (bigint 1))))

(defn contains-streak?
  "Returns true if a streak of n consecutive e exists in s falsey otherwise."
  [s e n]
  (assert (sequential? s) "Cannot test contains-streak? on a non-seq.")
  (loop [l s
         c 0]
    (cond (= n c) true
          (empty? l) nil
          (= (first l) e) (recur (rest l) (inc c))
          :else (recur (rest l) 0))))

(defn sum
  [s]
  (reduce + s))
