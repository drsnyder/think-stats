(ns think-stats.util
  (:require [clojure.java.io  :as io]
            [lonocloud.synthread :as ->]))

(defn str-to-int
  [s]
  (when (not (empty? s))
    (Integer/parseInt s)))

(defn str-to-float
  [s]
  (when (not (empty? s))
    (Float/parseFloat s)))

(defn read-file
  [file &{:keys [gunzip] :or {gunzip false}}]
  (with-open [in (-> (io/input-stream file)
                 (->/when gunzip
                   (java.util.zip.GZIPInputStream.))
                 (io/reader))]
    (doall 
      (line-seq in))))




