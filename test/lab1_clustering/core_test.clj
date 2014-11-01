(ns lab1-clustering.core-test
  (:use clojure.test
        lab1-clustering.core
        lab1-clustering.maths))

(deftest euclid-distance-nd-test
  (testing "Distances mismatch."
    (is (= 3 (euclid-distance-nd [1, 1] [4, 1])))))
