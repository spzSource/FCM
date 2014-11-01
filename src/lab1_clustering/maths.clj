(ns lab1-clustering.maths
	(:require [clojure.math.numeric-tower :as math]))

; (defn euclid-distance-nd [a, b]
; 	(math/sqrt(reduce + (map #(math/pow (- %1 %2) 2) a b))))

(defn- sqr [value]
	(math/expt value 2))

(defn- euclid-squared-distance [a, b]
	(reduce + (map (comp sqr -) a, b)))

(defn euclid-distance-nd [a, b]
	(math/sqrt (euclid-squared-distance a, b)))