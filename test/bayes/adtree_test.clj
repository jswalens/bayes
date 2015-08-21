(ns bayes.adtree-test
  (:require [clojure.test :refer :all]
            [bayes.adtree]))

; Note: @#'bayes.adtree/drop-one is a trick to get the private function drop-one

(deftest drop-one
  (are [coll i expected] (= expected (@#'bayes.adtree/drop-one i coll))
    [0 1 2] 0 [1 2]
    [0 1 2] 1 [0 2]
    [0 1 2] 2 [0 1]
    [0 1 2] 3 [0 1 2]
    []      0 []))

(deftest swap-bit
  (is (= 0 (@#'bayes.adtree/swap-bit 1)))
  (is (= 1 (@#'bayes.adtree/swap-bit 0))))
