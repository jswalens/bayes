(ns bayes.learner
  (:require [bayes.net :as net]))

(def ^:const QUERY_VALUE_WILDCARD -1)

(defn alloc [data adtree]
  "Allocate the learner.

  In the C version, parts of this struct are aligned to cache lines. We don't
  care about that here."
  {:adtree                     adtree
   :net                        (net/alloc (:n-var data))
   :local-base-log-likelihoods [] ; will contain (:n-var data) floats
   :base-log-likelihood        0.0
   :tasks                      [] ; will contain (:n-var data) learner tasks
   :task-list                  (list) ; TODO: sorted by compareTask
   :n-total-parent             0})

(defn- populate-query-vectors [net id queries]
  "TODO"
  [[] []])

(defn- compute-local-log-likelihood [id adtree queries query-vector parent-query-vector]
  "TODO"
  [0.0 []])

(defn- sum [ns]
  "Sums `ns`."
  (reduce + ns))

(defn score [learner]
  (let [n-var
          (:n-var (:adtree learner))
        initial-queries
          (for [v (range n-var)]
            {:index v :value QUERY_VALUE_WILDCARD})
        n-total-parent
          (sum
            (map
              (fn [v] (count (net/get-parent-id-list (:net learner) v)))
              (range n-var)))
        log-likelihood
          (first
            (reduce
              (fn [[log-likelihood queries] v]
                (let [[query-vector parent-query-vector]
                        (populate-query-vectors (:net learner) v queries)
                      [local-log-likelihood updated-queries]
                        (compute-local-log-likelihood
                          v
                          (:adtree learner)
                          queries
                          query-vector
                          parent-query-vector)]
                [(+ log-likelihood local-log-likelihood) updated-queries]))
              [0 initial-queries]
              (range n-var)))
        n-record
          (:n-record (:adtree learner))
        penalty
          (* -0.5 n-total-parent (Math/log n-record))
        score
          (+ penalty (* n-record log-likelihood))]
    score))
