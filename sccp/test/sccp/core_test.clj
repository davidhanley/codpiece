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

(defn every[x](reduce (fn[x y](and x y)) x))     
     
(def base-perft [ 20 400 8902 197281 4865609 119060324 3195901860 84998978956 ] )

(defn test-perft[]
     (map (fn[d ct](= ct (perft d start-board))) (range 1 100) base-perft))

; r3k2r/p1ppqpb1/bn2pnp1/3PN3/1p2P3/2N2Q1p/PPPBBPPP/R3K2R w KQkq -
; 48  2039 97862 4085603 193690690

(deftest perft-movegen
  (testing "perft from the start position"
	   (is (every (map (fn[d ct](= ct (perft start-board d))) (range 4) base-perft)))))
