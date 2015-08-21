(ns bayes.adtree-test
  (:require [clojure.test :refer :all]
            [bayes.adtree :as adtree]))

(def example-data
  {:n-var 2 :n-record 4 :records [[0 0] [0 1] [1 0] [1 1]]})

(def example-adtree
  (adtree/make example-data))

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

(deftest get-count
  ; TODO: I don't know whether these are the expected results
  (is (= 2 (adtree/get-count example-adtree
    [{:index 0 :value 0}]
    [0])))
  (is (= 2 (adtree/get-count example-adtree
    [{:index 0 :value 1}]
    [0])))
  (is (= 2 (adtree/get-count example-adtree
    [{:index 0 :value 0} {:index 1 :value 1}]
    [0])))
  (is (= 2 (adtree/get-count example-adtree
    [{:index 0 :value 0} {:index 1 :value 1}]
    [1])))
  (is (= 2 (adtree/get-count example-adtree
    [{:index 0 :value 1} {:index 1 :value 1}]
    [0])))
  (is (= 2 (adtree/get-count example-adtree
    [{:index 0 :value 1} {:index 1 :value 1}]
    [1])))
  (is (= 1 (adtree/get-count example-adtree
    [{:index 0 :value 0} {:index 1 :value 1}]
    [0 1])))
  (is (= 4 (adtree/get-count example-adtree
    [{:index 0 :value 0} {:index 1 :value 1}]
    []))))
