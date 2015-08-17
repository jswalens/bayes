(ns bayes.learner
  (:require [bayes.net :as net]
            [bayes.adtree :as adtree]))

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

; queries are a vector of maps {:index ... :value ...}, e.g:
; [{:index 0 :value -1} {:index 1 :value 1} {:index 2 :value 0} ...]
; where :index of query i is always i (I think XXX), and value is either 0, 1,
; or QUERY_VALUE_WILDCARD (-1).
; This corresponds to the queries of the C version, which is an array of
; structs.
;
; TODO: is it still necessary to keep the index in the query for the Clojure
; version?

(defn- sort-queries [queries]
  "Sort a list of queries by their index."
  (sort-by :index queries))

(defn- set-query-value [queries index value]
  "Set value of query at `index` in `queries` to `value`."
  (assoc-in queries [index :value] value))

; In the C version, a query-vector or a parent-query-vector is a vector of
; pointers to a query. Therefore, changing a query's value is reflected in all
; (parent-)query-vectors in which it is included.
; In the Clojure version, a (parent-)query-vector is a list of indices. This
; means that whenever we want to retrieve a query from a (parent-)query-vector,
; we have an extra bit of indirection.

(defn- populate-parent-query-vector [net id queries]
  (net/get-parent-id-list net id))

(defn- populate-query-vectors [net id queries]
  (let [parent-query-vector (populate-parent-query-vector net id queries)
        query-vector        (sort-queries (conj parent-query-vector id))]
    [query-vector parent-query-vector]))

(defn- compute-specific-local-log-likelihood [adtree queries query-vector parent-query-vector]
  (let [count (adtree/get-count adtree queries query-vector)]
    (if (= count 0)
      0.0
      (let [probability (/ (double count) (double (:n-record adtree)))
            parent-count (adtree/get-count adtree queries parent-query-vector)]
        (* probability (Math/log (/ (double count) (double parent-count))))))))

(defn- compute-local-log-likelihood-helper [i adtree queries query-vector parent-query-vector]
  (if (>= i (count parent-query-vector))
    (compute-specific-local-log-likelihood adtree queries query-vector parent-query-vector)
    (+
      (compute-local-log-likelihood-helper (inc i) adtree
        (set-query-value queries (nth parent-query-vector i) 0)
        query-vector parent-query-vector)
      (compute-local-log-likelihood-helper (inc i) adtree
        (set-query-value queries (nth parent-query-vector i) 1)
        query-vector parent-query-vector))))

(defn- compute-local-log-likelihood [id adtree queries query-vector parent-query-vector]
  (+
    (compute-local-log-likelihood-helper 0 adtree (set-query-value queries id 0)
      query-vector parent-query-vector)
    (compute-local-log-likelihood-helper 0 adtree (set-query-value queries id 1)
      query-vector parent-query-vector)))

(defn- sum [ns]
  "Sums `ns`."
  (reduce + ns))

(defn score [learner]
  (let [n-var
          (:n-var (:adtree learner))
        queries
          (vec (for [v (range n-var)]
            {:index v :value QUERY_VALUE_WILDCARD}))
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
