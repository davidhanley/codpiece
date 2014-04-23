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
