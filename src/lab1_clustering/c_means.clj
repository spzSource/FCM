(ns lab1-clustering.c-means
	(:gen-class)
	(:require [clojure.math.numeric-tower :as math])
	(:use lab1-clustering.distance)
	(import java.lang.Math))

(defstruct point-info :coord :potential)
(defstruct potential-bounds :lower-bound :upper-bound)

(defn- distance-measure [coef, coord1, coord2, fn-distance]
	(Math/exp(- (* coef (fn-distance coord1, coord2)))))

(defn- initial-info [alpha, coords, coord, fn-distance]
	(let [measure (reduce + (map #(distance-measure alpha, coord, %, fn-distance), coords))]
		(struct-map point-info 
			:coord coord,
			:potential measure)))

(defn points-infos [alpha, coords, fn-distance]
	(map #(initial-info alpha, coords, %, fn-distance), coords))

(defn- revised-potential [p-potent, c-potent, beta, p-coord, c-coord, fn-distance]
	(- p-potential (* c-potential (distance-measure beta, p-coord, c-coord, fn-distance))))

(defn- revised-info [beta, p-info, c-info, fn-distance]
	; use (:coord p-info) instead (accessor p-info :coord)
 	(let [p-coord (accessor p-info :coord)
 		  c-coord (accessor p-info :coord)
 		  p-potential (accessor p-info :potential)
 		  c-potential (accessor p-info :potential)]
 		  (let [p-revised (revised-potential p-potential, c-potential, beta, p-coord, c-coord, fn-distance)]
 		  	(struct-map point-info 
				:coord p-coord,
				:potential p-revised))))

(defn- recalculate_infos [beta, infos, c-info, fn-distance]
	(map #(revised-info beta, %, c-info, fn-distance), infos))

(defn determine-cluster-center [infos]
	(apply max-key (fn [x] (:potential x)) infos))

(defn- potential-bounds [potential, eps-l, eps-h]
	(struct-map potential-bounds
		:lower-bound (* potential eps-l)
		:upper-bound (* potential eps-h)))

(defn- potential-greater-than-upper-bound [center-info, bounds]
	(> (:potential center-info) (:upper-bound bounds)))

(defn- potential-less-than-lower-bound [center-info, bounds]
	(< (:potential center-info) (:lower-bound bounds)))

(defn make-clusterization [coords, fn-distance, alpha, beta, eps-l, eps-h]
 	(let [start-infos (points-infos alpha, coords, fn-distance)]
 		(let [first-center (determine-cluster-center start-infos)]
 			(loop [centers [first-center] infos (start-infos)] 
 				(let [revised-infos (recalculate_infos beta, infos, (first centers), fn-distance)]
 					(let [new-center (determine-cluster-center revised-infos)]
 						(let [bounds (potential-bounds (:potential new-center), eps-l, eps-h)])))))))