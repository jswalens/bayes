(ns bayes.learner-test
  (:require [clojure.test :refer :all]
            [bayes.net :as net]
            [bayes.learner :as learner]))

(deftest add-task
  (are [tasks task expected] (= expected (@#'bayes.learner/add-task tasks task))
    (ref [{:op 1 :score 1} {:op 3 :score 3}]) {:op 5 :score 5}
      [{:op 1 :score 1} {:op 3 :score 3} {:op 5 :score 5}]
    (ref [{:op 1 :score 1} {:op 3 :score 3}]) {:op 2 :score 2}
      [{:op 1 :score 1} {:op 2 :score 2} {:op 3 :score 3}]))

(deftest add-tasks
  (is (=
    [{:op 1 :score 1} {:op 2 :score 2} {:op 3 :score 3} {:op 4 :score 4}]
    (@#'bayes.learner/add-tasks
      (ref [{:op 1 :score 1} {:op 3 :score 3}])
      [{:op 2 :score 2} {:op 4 :score 4}]))))

(deftest pop-task
  (let [tasks (ref [{:op 1 :score 1} {:op 2 :score 2} {:op 3 :score 3}])]
    (is (= {:op 3 :score 3} (@#'bayes.learner/pop-task tasks)))
    (is (= [{:op 1 :score 1} {:op 2 :score 2}] @tasks))))

(deftest set-query-value
  (is (= [{:index 0 :value 10} {:index 1 :value 21} {:index 2 :value 12}]
    (@#'bayes.learner/set-query-value
      [{:index 0 :value 10} {:index 1 :value 11} {:index 2 :value 12}]
      1
      21))))

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

(deftest populate-parent-query-vector
  (are [net id pqv] (= pqv (@#'bayes.learner/populate-parent-query-vector net id))
    linear-net 0  (list)
    linear-net 1  (list 0)
    linear-net 2  (list 1)
    central-net 0 (list)
    central-net 1 (list 0 2)
    central-net 2 (list)))

(deftest populate-query-vectors
  (are [net id pqv qv] (= [qv pqv] (@#'bayes.learner/populate-query-vectors net id))
    linear-net 0  (list)     (list 0)
    linear-net 1  (list 0)   (list 0 1)
    linear-net 2  (list 1)   (list 1 2)
    central-net 0 (list)     (list 0)
    central-net 1 (list 0 2) (list 0 1 2)
    central-net 2 (list)     (list 2)))
