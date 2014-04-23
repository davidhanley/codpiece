(ns sccp.core-test
  (:require [clojure.test :refer :all]
            [sccp.core :refer :all]))

(deftest basic-square-test
  (testing "basic square stuff"
	   (is (= (square-to-string 45) "f3"))
	   (is (axis-ok 1))
	   (is (not (axis-ok -1)))
	   (is (axis-ok 7))
	   (is (not (axis-ok 8)))
	   )
  )

(deftest simple-piece
  (testing "basic piece move"
	   (is (= [[1 0] [0 1] [1 1]] (hopper-to [0 0] king-deltas)))
	   (is (= [[1 2] [2 1]] (hopper-to [0 0] knight-deltas)))
	   )   
  )   