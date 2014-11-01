(ns lab1-clustering.c-means
	(:gen-class)
	(:require [clojure.math.numeric-tower :as math])
	(:use lab1-clustering.distance)
	(import java.lang.Math))


(defn- distance-measure [alpha, coord1, coord2, fn-distance]
	(Math/exp(- (* alpha (fn-distance coord1, coord2)))))

(defn- point-potential [alpha, coords, coord, fn-distance]
	(let [measure (reduce + (map #(distance-measure alpha, coord, %, fn-distance), coords))]
		(list coord, measure)))

(defn points-potentials [alpha, coords, fn-distance]
	(map #(point-potential alpha, coords, %, fn-distance), coords))
