(ns bayes.data-test
  (:require [clojure.test :refer :all]
            [bayes.data :refer :all]))

; Note: @#'bayes.data/bits->bitmap is a trick to get the private function
; bits->bitmap.

(deftest bits->bitmap
  (are [expected in] (= expected (@#'bayes.data/bits->bitmap in))
    2r0   (list 0)
    2r1   (list 1)
    2r10  (list 1 0)
    2r100 (list 1 0 0)
    2r101 (list 1 0 1)
    2r1   (list 0 0 1)
    2r10  (list 0 1 0)))

(deftest concat-uniq
  (are [xs ys expected] (= expected (@#'bayes.data/concat-uniq xs ys))
    [1 2 3] [4 5 6] [1 2 3 4 5 6]
    [1 2 3] [1 5 6] [1 2 3 5 6]
    [1 2 3] [1 3 3] [1 2 3]
    []      [4 5 6] [4 5 6]
    [1 2 3] []      [1 2 3]))

(deftest compare-record
  (are [a b offset expected] (= expected (@#'bayes.data/compare-record a b offset))
    [0 0 0] [0 0 0] 0 0  ; equal
    [0 0 0] [1 0 0] 0 -1 ; smaller
    [1 0 0] [0 0 0] 0 +1 ; larger
    [1 0 0] [1 0 0] 0 0
    [0 0 0] [0 0 1] 0 -1
    [0 0 0] [0 0 1] 1 -1
    [0 0 0] [0 0 1] 2 -1
    [0 0 0] [0 1 0] 0 -1
    [0 0 0] [0 1 0] 1 -1
    [0 0 0] [0 1 0] 2 0
    [0 0 0] [1 0 0] 0 -1
    [0 0 0] [1 0 0] 1 0
    [0 0 0] [1 0 0] 2 0))

(deftest sort-records
  (are [records start n offset expected]
    (= expected (@#'bayes.data/sort-records records start n offset))
    [[0 1] [1 1] [1 0] [0 0]] 0 4 0 (list [0 0] [0 1] [1 0] [1 1])
    ; change offset:
    [[0 1] [1 1] [1 0] [0 0]] 0 4 1 (list [1 0] [0 0] [0 1] [1 1])
    ; change start (records 1, 2, 3):
    [[0 1] [1 1] [1 0] [0 0]] 1 3 0 (list [0 1] [0 0] [1 0] [1 1])
    ; change n (records 0, 1):
    [[0 1] [1 1] [1 0] [0 0]] 0 2 0 (list [0 1] [1 1] [1 0] [0 0])
    ; change start and n (records 1, 2):
    [[0 1] [1 1] [1 0] [0 0]] 1 2 0 (list [0 1] [1 0] [1 1] [0 0])
    ; change start, n (records 1, 2), and offset:
    [[0 1] [0 1] [1 0] [0 0]] 1 2 1 (list [0 1] [1 0] [0 1] [0 0])))
