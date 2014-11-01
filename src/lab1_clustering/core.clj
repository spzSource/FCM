(ns lab1-clustering.core
    (:gen-class)
  	(import java.lang.Math)
    (:use lab1-clustering.delimited-readers))

(def ^:const first_path "src/lab1_clustering/data/bezdekIris.data.txt")
(def ^:const delimiter #",")

(defn -main [& args]
  	(println(retrieve-data first_path, delimiter)))
