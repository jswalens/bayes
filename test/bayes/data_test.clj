(ns bayes.data-test
  (:require [clojure.test :refer :all]
            [bayes.data :refer :all]))

(deftest bits->bitmap-test
  (are [expected in] (= expected (@#'bayes.data/bits->bitmap in))
    2r0   (list 0)
    2r1   (list 1)
    2r10  (list 1 0)
    2r100 (list 1 0 0)
    2r101 (list 1 0 1)
    2r1   (list 0 0 1)
    2r10  (list 0 1 0)))
