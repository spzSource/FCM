(ns lab1-clustering.delimited-readers
	(:require [clojure.java.io :as io]
              [clojure.string  :as str]))
	
(defn to-list [line, delimiter] 
    (map read-string (drop-last(str/split line delimiter))))

(defn retrieve-data [path, delimiter]
  	(with-open [reader (io/reader path)]
    	(let [lines (line-seq reader)]
            (doall (map #(to-list %, delimiter) lines)))))