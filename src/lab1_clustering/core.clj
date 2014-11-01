(ns lab1-clustering.core
    (:gen-class)
  	(:require [clojure.java.io :as io]
              [clojure.string  :as str])
  	(import java.lang.Math))

(def ^:const first_path "src/lab1_clustering/data/bezdekIris.data.txt")
(def ^:const delimiter #",")

(defn to-list [line, delimiter] 
    (map read-string (drop-last(str/split line delimiter))))

(defn retrieve-data [path, delimiter]
  	(with-open [reader (io/reader path)]
    	(let [lines (line-seq reader)]
            (doall (map #(to-list %, delimiter) lines)))))

(defn -main [& args]
  	(println(retrieve-data first_path, delimiter)))
