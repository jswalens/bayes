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

(defn- sort-queries [queries]
  "Sort a list of queries by their index."
  (sort-by :index queries))

(defn- populate-parent-query-vector [net id queries]
  (map #(nth queries %) (net/get-parent-id-list net id)))

(defn- populate-query-vectors [net id queries]
  (let [parent-query-vector (populate-parent-query-vector net id queries)
        query-vector (sort-queries (conj parent-query-vector (nth queries id)))]
    [query-vector parent-query-vector]))

(defn- compute-local-log-likelihood [id adtree queries query-vector parent-query-vector]
  "TODO"
  0.0)

(defn- sum [ns]
  "Sums `ns`."
  (reduce + ns))

(defn score [learner]
  (let [n-var
          (:n-var (:adtree learner))
        queries
          (for [v (range n-var)]
            {:index v :value QUERY_VALUE_WILDCARD})
        n-total-parent
          (sum
            (map
              (fn [v] (count (net/get-parent-id-list (:net learner) v)))
              (range n-var)))
        log-likelihood
          (sum
            (map
              (fn [v]
                (let [[query-vector parent-query-vector]
                        (populate-query-vectors (:net learner) v queries)]
                  (compute-local-log-likelihood
                    v
                    (:adtree learner)
                    queries
                    query-vector
                    parent-query-vector)))
              (range n-var)))
        n-record
          (:n-record (:adtree learner))
        penalty
          (* -0.5 n-total-parent (Math/log n-record))
        score
          (+ penalty (* n-record log-likelihood))]
    score))
