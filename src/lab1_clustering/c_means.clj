(ns lab1-clustering.c-means
	(:gen-class)
	(:require [clojure.math.numeric-tower :as math])
	(:use lab1-clustering.distance)
	(import java.lang.Math))

(defstruct point-info :coord :potential)

(defn- distance-measure [coef, coord1, coord2, fn-distance]
	(Math/exp(- (* coef (fn-distance coord1, coord2)))))

(defn- initial-info [alpha, coords, coord, fn-distance]
	(let [measure (reduce + (map #(distance-measure alpha, coord, %, fn-distance), coords))]
		(struct-map point-info 
			:coord coord,
			:potential measure)))

(defn points-infos [alpha, coords, fn-distance]
	(map #(initial-info alpha, coords, %, fn-distance), coords))


(defn- revised-infos [beta, p-info, c-info, fn-distance]
 	(let [p-coord (accessor p-info :coord)
 		  c-coord (accessor p-info :coord)
 		  p-potential (accessor p-info :potential)
 		  c-potential (accessor p-info :potential)]
 		  (let [p-revised (- p-potential (* c-potential (distance-measure beta, p-coord, c-coord, fn-distance)))]
 		  	(struct-map point-info 
				:coord p-coord,
				:potential p-revised))))

(defn determine-first-cluster [initial-infos]
	(apply max-key (fn [x] (:potential x)) initial-infos))

 ; (defn make-clusterization [coords, fn-distance, alpha, beta]
 ; 	(let [start-potentials (points-infos alpha, coords, fn-distance)]
 ; 		()))