(ns bayes.data-test
  (:require [clojure.test :refer :all]
            [random]
            [bayes.main :as main]
            [bayes.data :as data]))

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

(def generated-data
  {:data {:n-var 3
          :n-record 4
          :records '([1 0 0] [1 0 1] [0 0 0] [0 1 0])}
   :net  [{:id 0 :parent-ids '()  :child-ids '()}
          {:id 1 :parent-ids '(2) :child-ids '()}
          {:id 2 :parent-ids '()  :child-ids '(1)}]})

(defn- deref-net [net]
  (for [n net]
    (-> n
      (update-in [:parent-ids] deref)
      (update-in [:child-ids] deref))))

(deftest generate
  (random/set-seed 1)
  (let [params (assoc main/fast-params :record 4 :var 3)
        {data :data net :net} (data/generate params)]
    (is (= (:data generated-data) data))
    (is (= (:net  generated-data) (deref-net net)))))

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

(def sort-cases
  [; records              start n offset  result
   [[[0 1] [1 1] [1 0] [0 0]] 0 4 0 (list [0 0] [0 1] [1 0] [1 1])]
   ; change offset:
   [[[0 1] [1 1] [1 0] [0 0]] 0 4 1 (list [1 0] [0 0] [0 1] [1 1])]
   ; change start (records 1, 2, 3):
   [[[0 1] [1 1] [1 0] [0 0]] 1 3 0 (list [0 1] [0 0] [1 0] [1 1])]
   ; change n (records 0, 1):
   [[[0 1] [1 1] [1 0] [0 0]] 0 2 0 (list [0 1] [1 1] [1 0] [0 0])]
   ; change start and n (records 1, 2):
   [[[0 1] [1 1] [1 0] [0 0]] 1 2 0 (list [0 1] [1 0] [1 1] [0 0])]
   ; change start, n (records 1, 2), and offset:
   [[[0 1] [0 1] [1 0] [0 0]] 1 2 1 (list [0 1] [1 0] [0 1] [0 0])]])

(deftest sort-records
  (doseq [[records start n offset result] sort-cases]
    (is (= result (@#'bayes.data/sort-records records start n offset)))))

(deftest sort
  (doseq [[records start n offset result] sort-cases]
    (is (= {:n-var 2 :n-record 4 :records result}
      (bayes.data/sort {:n-var 2 :n-record 4 :records records} start n offset)))))

(deftest find-split
  (are [data start n offset expected] (= expected (bayes.data/find-split data start n offset))
    {:records [[0 0] [0 1] [1 0] [1 1]]} 0 4 0  2 ; first two start with 0, next two start with 1
    ; change offset:
    {:records [[1 0] [0 0] [0 1] [1 1]]} 0 4 1  2 ; first two have 0, next two have 1, at offset 1
    ; change start (records 1, 2, 3):
    {:records [[0 1] [0 0] [1 0] [1 1]]} 1 3 0  1 ; first one start with 0, next two start with 1
    ; change n (records 0, 1):
    {:records [[0 1] [1 1] [1 0] [0 0]]} 0 2 0  1 ; first one starts with 0, next one starts with 1
    ; change start and n (records 1, 2):
    {:records [[0 1] [1 0] [1 1] [0 0]]} 1 2 0  0 ; none start with zero, two start with 1
    ; change start, n (records 1, 2), and offset:
    {:records [[0 1] [1 0] [0 1] [0 0]]} 1 2 1  1)) ; first one has zero, next one has 1, at offset 1
