(ns lab1-clustering.c-means-cluster
	(import java.lang.Math))

(defn- distance-measure [alpha, coords, coord, fn-distance]
	(Math/exp(- (* alpha (fn-distance coord-nd)))))

(defn- point-potential [alpha, coords, coord, fn-distance]
	(let [measure (reduce + (map #(distance-measure alpha, coord, %, fn-distance), coords))]
		(list coord, measure))

(defn points-potentials [alpha, coords, fn-distance]
	(map (point-potential alpha, coords, %, fn-distance), coords))