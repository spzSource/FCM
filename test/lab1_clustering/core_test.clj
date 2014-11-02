(ns lab1-clustering.core-test
  (:use clojure.test
        lab1-clustering.core
        lab1-clustering.distance
        lab1-clustering.c-means))

(deftest euclid-distance-nd-test
  (testing "Distances mismatch."
    (is (= 3 (euclid-distance-nd [1, 1] [4, 1])))))


(deftest deternining-first-cluster-test
  (testing "Invalid first cluster definition."
  	(is (= 10 (:potential (determine-first-cluster [
	  		(struct-map point-info :coord '(1, 2, 3) :potential 10),
			(struct-map point-info :coord '(3, 2, 1) :potential 2),
			(struct-map point-info :coord '(10, 1, 30) :potential 1)]))))))
