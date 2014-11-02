(ns lab1-clustering.c-means
	(:gen-class)
	(:require [clojure.math.numeric-tower :as math])
	(:use lab1-clustering.distance)
	(import java.lang.Math))

(defstruct point-info :index :coord :potential)
(defstruct potential-bounds :lower-bound :upper-bound)


(defn- distance-measure [coef, coord1, coord2, fn-distance]
	(Math/exp(- (* coef (fn-distance coord1, coord2)))))


(defn- initial-info [alpha, coords, coord, fn-distance, p-index]
	(let [measure (reduce + (map #(distance-measure alpha, coord, %, fn-distance), coords))]
		(struct-map point-info 
			:index p-index
			:coord coord,
			:potential measure)))


(defn points-infos [alpha, coords, fn-distance]
	(map-indexed (fn [i, e] (initial-info alpha, coords, e, fn-distance, (inc i))), coords))


(defn- revised-potential [p-potent, c-potent, beta, p-coord, c-coord, fn-distance]
	(- p-potent (* c-potent (distance-measure beta, p-coord, c-coord, fn-distance))))


(defn- revised-info [beta, p-info, c-info, fn-distance]
 	(let [p-index (:index p-info)
 		  c-index (:index c-info)
 		  p-coord (:coord p-info)
 		  c-coord (:coord c-info)
 		  p-potential (:potential p-info)
 		  c-potential (:potential c-info)]
 		  (let [p-revised (revised-potential p-potential, c-potential, beta, p-coord, c-coord, fn-distance)]
 		  	(struct-map point-info
 		  		:index p-index 
				:coord p-coord,
				:potential p-revised))))


(defn- recalculate_infos [beta, infos, c-info, fn-distance]
	(map #(revised-info beta, %, c-info, fn-distance), infos))


(defn determine-cluster-center [infos]
	(apply max-key (fn [x] (:potential x)) infos))


(defn- calc-potential-bounds [potential, eps-l, eps-h]
	(struct-map potential-bounds
		:lower-bound (* potential eps-l)
		:upper-bound (* potential eps-h)))


(defn- potential-greater-than-upper-bound? [center-info, bounds]
	(> (:potential center-info) (:upper-bound bounds)))


(defn- potential-less-than-lower-bound? [center-info, bounds]
	(< (:potential center-info) (:lower-bound bounds)))


(defn- calculate-min-distance [center-info, existing-centers, fn-distance]
	(apply min (map #(fn-distance (:coord center-info) (:coord %)), existing-centers)))


(defn- need-new-center? [center-info, existing-centers, first-center, ra, fn-distance]
	(let [min-distance (calculate-min-distance center-info, existing-centers, fn-distance)]
		(>= (+ (/ min-distance ra) (/ (:potential center-info) (:potential first-center)))) 1))


(defn- remove-center-from-infos [center-info, infos]
	(remove #(= (:index center-info) :index %), infos))


(defn- update-potential-for-rejected-center [info, infos, p-value]
	(map #(if (= (:index info) (:index %)) (assoc info :potential p-value) %), infos))


(defn make-clusterization [coords, fn-distance, alpha, beta, eps-l, eps-h, ra]
 	(let [start-infos (points-infos alpha, coords, fn-distance)]
 		(let [first-center (determine-cluster-center start-infos)]
 			(loop [centers [first-center] infos start-infos] 
 				; (println #"---------------------------------------------")
 				; (doseq [info infos]
 				; 	(println info))
 				; (println #"---------------------------------------------")
 				(let [revised-infos (recalculate_infos beta, infos, (first centers), fn-distance)]
 					(let [new-center (determine-cluster-center revised-infos)]
 						(let [bounds (calc-potential-bounds (:potential first-center), eps-l, eps-h)]
 							; (println new-center)
 							(cond
 								(potential-greater-than-upper-bound? new-center, bounds) 
 									(recur (cons new-center centers), revised-infos);(remove-center-from-infos new-center, revised-infos))
 								(potential-less-than-lower-bound? new-center, bounds)
 									centers
 								(need-new-center? new-center, centers, first-center, ra, fn-distance) 
 									(recur (cons new-center centers), revised-infos);(remove-center-from-infos new-center, revised-infos))
 								:else (recur centers, (update-potential-for-rejected-center new-center, revised-infos, 0))))))))))