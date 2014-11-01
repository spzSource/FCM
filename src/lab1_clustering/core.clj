(ns lab1-clustering.core
    (:gen-class)
  	(import java.lang.Math)
    (:use lab1-clustering.delimited-readers)
    (:use lab1-clustering.c-means)
    (:use lab1-clustering.distance))

(def ^:const first_path "src/lab1_clustering/data/bezdekIris.data.txt")
(def ^:const delimiter #",")

(def ra 3)

(def alpha 
	(/ 4 (* ra ra)))


(defn -main [& args]
	(let [coords (retrieve-data first_path, delimiter)]
		(println(points-potentials alpha, coords, euclid-distance-nd))))
