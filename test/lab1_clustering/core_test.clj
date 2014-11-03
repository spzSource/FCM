(ns lab1-clustering.core-test
  	(:use clojure.test
        lab1-clustering.core
        lab1-clustering.distance
        lab1-clustering.c-means))

;
; @euclid-distance-nd
;
(deftest euclid-distance-nd-test
  	(testing "Euclid distances mismatch."
    	(is (= 3 (euclid-distance-nd [1, 1] [4, 1])))))

;
; @hamming-distance-nd
;
(deftest hamming-distance-nd-test
	(testing "Hamming distance mismatch."
		(is (= 3 (hamming-distance-nd [1, 0, 1, 1, 1], [1, 1, 0, 0, 1])))))

;
; @calc-potential-bounds
;
(deftest calc-potential-bounds-test
	(testing "Potential bounds mismatch."
		(is (true?
				(let [bounds (calc-potential-bounds 10, 0.15, 0.5)]
					(and (= (:lower-bound bounds) 1.5) (= (:upper-bound bounds) 5.0)))))))
;
; @determine-cluster-center
;
(deftest determine-cluster-center-test
  	(testing "Invalid first cluster definition."
  		(is (= 10 (:potential (determine-cluster-center [
	  		(struct-map point-info :coord '(1, 2, 3) :potential 10),
			(struct-map point-info :coord '(3, 2, 1) :potential 2),
			(struct-map point-info :coord '(10, 1, 30) :potential 1)]))))))

;
; @calculate-min-distance (used fn euclid-distance-nd)
;
(deftest calculate-min-distance-test
  	(testing "Min distance mismatch."
  		(is (= 3 
  				(let [center (struct-map point-info :coord '(1, 1) :potential 10)
  			  	  	infos [(struct-map point-info :coord '(4, 1) :potential 10),
					 	(struct-map point-info :coord '(6, 1) :potential 2),
					 	(struct-map point-info :coord '(8, 1) :potential 1)]]
					(calculate-min-distance center, infos, euclid-distance-nd))))))

;
; @remove-center-from-infos 
;
(deftest remove-center-from-infos-test
  	(testing "Count of remaining infos mismatch."
  		(is (= 2 
  				(let [center (struct-map point-info :index 1 :coord '(1, 1) :potential 10)
  			  		  infos [(struct-map point-info :index 1 :coord '(1, 1) :potential 10),
						(struct-map point-info :index 3 :coord '(6, 1) :potential 2),
					 	(struct-map point-info :index 4 :coord '(8, 1) :potential 1)]]
			 		(let [new-infos (remove-center-from-infos center, infos)]
			 			(count new-infos)))))))

;
; @remove-center-from-infos 
;
(deftest update-potential-for-rejected-center-test
  	(testing "The value of potential for rejected center mismatch."
  		(is (let [p-value 0]
  				(= p-value 
  					(let [center (struct-map point-info :index 1 :coord '(1, 1) :potential 10)
  			  			  infos [(struct-map point-info :index 1 :coord '(1, 1) :potential 10),
							(struct-map point-info :index 3 :coord '(6, 1) :potential 2),
					 		(struct-map point-info :index 4 :coord '(8, 1) :potential 1)]]
			 			(let [new-infos (update-potential-for-rejected-center center, infos, p-value)]
			 				(:potential (first (filter #(= (:index %) (:index center)) new-infos))))))))))
