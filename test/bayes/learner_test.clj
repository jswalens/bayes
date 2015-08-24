(ns bayes.learner-test
  (:require [clojure.test :refer :all]
            [bayes.adtree :as adtree]
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

(def example-data
  {:n-var 3
   :n-record 4
   :records '([1 0 0] [1 0 1] [0 0 0] [0 1 0])})

(def example-net
  [{:id 0 :parent-ids '()  :child-ids '()}
   {:id 1 :parent-ids '(2) :child-ids '()}
   {:id 2 :parent-ids '()  :child-ids '(1)}])

(def example-adtree
  (adtree/make example-data))

(deftest compute-specific-local-log-likelihood
  ; TODO: don't know where these numbers come from
  (is (= -0.34657359027997264
    (@#'bayes.learner/compute-specific-local-log-likelihood
      example-adtree
      [{:index 0 :value 0} {:index 1 :value 1} {:index 2 :value 1}]
      (list 0)
      (list))))
  (is (= -0.17328679513998632
    (@#'bayes.learner/compute-specific-local-log-likelihood
      example-adtree
      [{:index 0 :value 0} {:index 1 :value 1} {:index 2 :value 1}]
      (list 0 1)
      (list 0))))
  (is (= 0.0
    (@#'bayes.learner/compute-specific-local-log-likelihood
      example-adtree
      [{:index 0 :value 0} {:index 1 :value 1} {:index 2 :value 1}]
      (list 1 2)
      (list 1)))))

(deftest compute-local-log-likelihood
  ; TODO: don't know where these numbers come from
  (is (= -0.6931471805599453
    (@#'bayes.learner/compute-local-log-likelihood
      0
      example-adtree
      [{:index 0 :value 0} {:index 1 :value 1} {:index 2 :value 1}]
      (list 0)
      (list))))
  (is (= -0.34657359027997264
    (@#'bayes.learner/compute-local-log-likelihood
      0
      example-adtree
      [{:index 0 :value 0} {:index 1 :value 1} {:index 2 :value 1}]
      (list 0 1)
      (list 0))))
  (is (= -0.5493061443340549
    (@#'bayes.learner/compute-local-log-likelihood
      0
      example-adtree
      [{:index 0 :value 0} {:index 1 :value 1} {:index 2 :value 1}]
      (list 1 2)
      (list 1))))
  (is (= -0.6931471805599453
    (@#'bayes.learner/compute-local-log-likelihood
      1
      example-adtree
      [{:index 0 :value 0} {:index 1 :value 1} {:index 2 :value 1}]
      (list 0)
      (list))))
  (is (= -0.34657359027997264
    (@#'bayes.learner/compute-local-log-likelihood
      1
      example-adtree
      [{:index 0 :value 0} {:index 1 :value 1} {:index 2 :value 1}]
      (list 0 1)
      (list 0))))
  (is (= -0.5493061443340549
    (@#'bayes.learner/compute-local-log-likelihood
      1
      example-adtree
      [{:index 0 :value 0} {:index 1 :value 1} {:index 2 :value 1}]
      (list 1 2)
      (list 1))))
  (is (= -0.6931471805599453
    (@#'bayes.learner/compute-local-log-likelihood
      2
      example-adtree
      [{:index 0 :value 0} {:index 1 :value 1} {:index 2 :value 1}]
      (list 0)
      (list))))
  (is (= -0.34657359027997264
    (@#'bayes.learner/compute-local-log-likelihood
      2
      example-adtree
      [{:index 0 :value 0} {:index 1 :value 1} {:index 2 :value 1}]
      (list 0 1)
      (list 0))))
  (is (= -0.4773856262211097  ; different than other id's
    (@#'bayes.learner/compute-local-log-likelihood
      2
      example-adtree
      [{:index 0 :value 0} {:index 1 :value 1} {:index 2 :value 1}]
      (list 1 2)
      (list 1)))))

(deftest sum
  (are [xs sum] (= sum (@#'bayes.learner/sum xs))
    [1 2 3]          6
    [0 1 2 7 3 28 3] 44
    []               0))

(def example-params
  {:thread  1
   :edge    1
   :insert  1
   :quality 1.0})

(def example-learner
  (learner/alloc example-adtree example-params))

(deftest score
  ; TODO: is this correct, does this make sense?
  (is (= -7.271269879190247 (learner/score example-learner))))
