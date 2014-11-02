(ns lab1-clustering.distance
	(:require [clojure.math.numeric-tower :as math]))

(defn- sqr [value]
	(math/expt value 2))

(defn- euclid-squared-distance [a, b]
	(reduce + (map (comp sqr -) a, b)))

(defn euclid-distance-nd [a, b]
	(math/sqrt (euclid-squared-distance a, b)))