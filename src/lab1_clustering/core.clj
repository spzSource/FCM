(ns lab1-clustering.core
    (:gen-class)
  	(import java.lang.Math)
    (:use lab1-clustering.delimited-readers)
    (:use lab1-clustering.c-means)
    (:use lab1-clustering.distance))

 ; (def ^:const first_path "src/lab1_clustering/data/bezdekIris.data.txt")
 (def ^:const delimiter #",")

 (def ra 3)
 (def rb (* 1.5 ra))
 (def alpha (/ 4 (* ra ra)))
 (def beta (/ 4 (* rb rb)))
 (def eps-l 0.15)
 (def eps-h 0.5)

(defn- determine-distance-fn 
	[fn-name]
	(case fn-name
		"hamming" hamming-distance-nd,
		"euclid" euclid-distance-nd))

(defn -main [& args]
	(let [file-path (first args)
		  ;delimiter (second args)
		  fn-distance  (determine-distance-fn (last args))
		  coords (retrieve-data file-path, delimiter)]
		(doseq [center (make-clusterization coords, fn-distance, alpha, beta, eps-l, eps-h, ra)]  
 			(println center))))