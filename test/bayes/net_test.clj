(ns bayes.net-test
  (:require [clojure.test :refer :all]
            [bayes.net :as net]))

(def linear-net
  "Net of three nodes with edges 0 -> 1 -> 2."
  (-> (net/alloc 3)
    (net/insert-edge 0 1)
    (net/insert-edge 1 2)))

(def central-net
  "Net of three nodes with edges 0 -> 1 <- 2."
  (-> (net/alloc 3)
    (net/insert-edge 0 1)
    (net/insert-edge 2 1)))

(deftest alloc-insert
  (is (=
    [{:id 0 :parent-ids []   :child-ids '(1)} ; 0 -> 1
     {:id 1 :parent-ids '(0) :child-ids '(2)} ; 0 -> 1 -> 2
     {:id 2 :parent-ids '(1) :child-ids []}]  ; 1 -> 2
    linear-net))
  (is (=
    [{:id 0 :parent-ids []     :child-ids '(1)}  ; 0 -> 1
     {:id 1 :parent-ids '(0 2) :child-ids []}    ; (0 2) -> 1, order is important!
     {:id 2 :parent-ids []     :child-ids '(1)}] ; 2 -> 1
    central-net)))

(deftest remove-edge
  (is (=
    [{:id 0 :parent-ids []   :child-ids '(1)} ; 0 -> 1
     {:id 1 :parent-ids '(0) :child-ids []}   ; 0 -> 1
     {:id 2 :parent-ids []   :child-ids []}]
    (net/remove-edge linear-net 1 2)))
  ; remove non-existing edge
  #_(is (= linear-net (net/remove-edge linear-net 0 2))))

(deftest reverse-edge
  (is (= central-net (net/reverse-edge linear-net 1 2)))
  (is (= linear-net (net/reverse-edge central-net 2 1))))

(deftest has-edge?
  (is (net/has-edge? linear-net 0 1))
  (is (net/has-edge? linear-net 1 2))
  (is (not (net/has-edge? linear-net 0 2))))

(deftest has-path?
  (is (net/has-path? linear-net 0 1))
  (is (net/has-path? linear-net 1 2))
  (is (net/has-path? linear-net 0 2))
  (is (not (net/has-path? linear-net 2 0)))
  (is (not (net/has-path? central-net 0 2))))

(deftest concat-uniq
  (are [xs ys expected] (= expected (@#'bayes.net/concat-uniq xs ys))
    [1 2 3] [4 5 6] [1 2 3 4 5 6]
    [1 2 3] [1 5 6] [1 2 3 5 6]
    [1 2 3] [1 3 3] [1 2 3]
    []      [4 5 6] [4 5 6]
    [1 2 3] []      [1 2 3]))

(deftest find-descendants
  (is (= #{1 2} (net/find-descendants linear-net 0)))
  (is (= #{2}   (net/find-descendants linear-net 1)))
  (is (= #{}    (net/find-descendants linear-net 2)))
  (is (= #{1}   (net/find-descendants central-net 0)))
  (is (= #{}    (net/find-descendants central-net 1)))
  (is (= #{1}   (net/find-descendants central-net 2))))
