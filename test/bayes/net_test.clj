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
