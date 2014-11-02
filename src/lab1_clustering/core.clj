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


(def test-data (struct-map point-info
            		:coord '(1, 2, 3),
            		:potential 10))

(defn get-potential [point-info]
	(:potential point-info))

 (defn -main [& args]
 	; (let [coords (retrieve-data first_path, delimiter)]
 	; 	(doseq [item (points-infos alpha, coords, euclid-distance-nd)]  
 	; 		(println item))))
	; (let [searched (determine-first-cluster 
 ;  				[(struct-map point-info
 ;            		:coord (1, 2, 3)
 ;            		:potential 10),
 ;  				(struct-map point-info
 ;            		:coord (3, 2, 1)
 ;            		:potential 2),
 ;  				(struct-map point-info
 ;            		:coord (10, 1, 30)
 ;            		:potential 1)])]
 ;  				(accessor searched :potential)
 ;  				(println searched)))



(let [s (apply max-key (fn [x] (:potential x)) [(struct-map point-info
					            		:coord '(1, 2, 3),
					            		:potential 10),
									  (struct-map point-info
					            		:coord '(1, 2, 3),
					            		:potential 11)])]
	(println s)))
; (get-potential (struct-map point-info
;             		:coord '(1, 2, 3),
;             		:potential 10)))