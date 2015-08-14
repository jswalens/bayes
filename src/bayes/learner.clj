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

(defn- sort-queries [queries]
  "Sort a list of queries by their index."
  (sort-by :index queries))

(defn- populate-parent-query-vector [net id queries]
  (map #(nth queries %) (net/get-parent-id-list net id)))

(defn- populate-query-vectors [net id queries]
  (let [parent-query-vector (populate-parent-query-vector net id queries)
        query-vector (sort-queries (conj parent-query-vector (nth queries id)))]
    [query-vector parent-query-vector]))

(defn- set-query-value [queries index value]
  "Set value of query at `index` in `queries` to `value`."
  (assoc-in queries [index :value] value))

; FIXME: set-query-value below should not only change them in queries, but also
; in query-vector and parent-query-vector, I think
;
; In other words, query-vector and parent-query-vector aren't copies of a subset
; of queries, but they point to these queries.
; A possible solution would be to store only the indices of the queries in
; query-vector and parent-vector, and look up their values using queries.

(defn- compute-specific-local-log-likelihood [adtree query-vector parent-query-vector]
  (let [count (adtree/get-count adtree query-vector)]
    (if (= count 0)
      0.0
      (let [probability (/ (double count) (double (:n-record adtree)))
            parent-count (adtree/get-count adtree parent-query-vector)]
        (* probability (Math/log (/ (double count) (double parent-count))))))))

(defn- compute-local-log-likelihood-helper [i adtree queries query-vector parent-query-vector]
  (if (>= i (count parent-query-vector))
    (compute-specific-local-log-likelihood adtree query-vector parent-query-vector)
    (+
      (compute-local-log-likelihood-helper (inc i) adtree
        (set-query-value queries (:index (nth parent-query-vector i)) 0)
        query-vector parent-query-vector)
      (compute-local-log-likelihood-helper (inc i) adtree
        (set-query-value queries (:index (nth parent-query-vector i)) 1)
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
