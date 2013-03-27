(ns think-stats.util
  (:require [clojure.java.io  :as io]
            [clojure.java.shell :as shell]
            [clojure.data.csv :as csv]
            [lonocloud.synthread :as ->]))

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


(defn read-file
  [file &{:keys [gunzip] :or {gunzip false}}]
  (with-open [in (-> (io/input-stream file)
                 (->/when gunzip
                   (java.util.zip.GZIPInputStream.))
                 (io/reader))]
    (doall 
      (line-seq in))))

(defn write-to-csv 
  "Write the simulation data to the given file."
  [file data & opts]
  (let [writer (if opts (apply io/writer file opts) (io/writer file))]
  (with-open [out-file writer] 
    (csv/write-csv out-file data))))



