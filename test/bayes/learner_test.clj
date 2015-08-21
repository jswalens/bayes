(ns bayes.learner-test
  (:require [clojure.test :refer :all]
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
