(ns bayes.learner
  (:require [bayes.net :as net]
            [bayes.adtree :as adtree]))

(def ^:const QUERY_VALUE_WILDCARD -1)

(defn alloc [data adtree n-thread]
  "Allocate the learner.

  We have an extra parameter :n-thread to contain the number of threads to use.

  The C version has tasks, an array containing tasks, and taskList, an ordered
  list of pointers to tasks (ordered by compareTask). We only have tasks, as an
  ordered list of tasks.

  In the C version, parts of this struct are aligned to cache lines. We don't
  care about that here."
  {:adtree                     adtree
   :net                        (net/alloc (:n-var data))
   :local-base-log-likelihoods [] ; will contain (:n-var data) floats
   :base-log-likelihood        (ref 0.0)
   :tasks                      (ref []) ; TODO: sorted by compareTask
   :n-total-parent             0
   :n-thread                   n-thread})

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
      (let [probability  (/ (double count) (double (:n-record adtree)))
            parent-count (adtree/get-count adtree queries parent-query-vector)]
        (* probability (double (Math/log (/ (double count) (double parent-count)))))))))

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

(defn- create-partition [minimum maximum i n]
  "Given a range from `minimum` to `maximum`, returns the subrange for chunk `i`
  out of `n` total chunks."
  (let [size  (- maximum minimum)            ; total range
        chunk (max 1 (/ (+ size (/ n 2)) n)) ; size of 1 chunk; integer math
        start (+ minimum (* chunk i))        ; start of this chunk
        stop  (if (= i (dec n))              ; end of this chunk
                maximum
                (min maximum (+ start chunk)))]
    (range start stop)))

(defn- compute-local-base-log-likelihoods [vars adtree]
  ; The C version has queries = [X, Y]; queryVector = [&X];
  ; parentQuery = Z; and parentQueryVector = [].
  ; In Clojure, we have queries = [X]; query-vector = [0];
  ; parent-query-vector = []
  (for [v vars]
    (+
      (compute-specific-local-log-likelihood adtree [{:index v :value 0}]
        [0] [])
      (compute-specific-local-log-likelihood adtree [{:index v :value 1}]
        [0] []))))

(defn- create-task [v adtree base-log-likelihood this-local-log-likelihood]
  "Create and return task for variable `v`, or nil if no better local log
  likelihood exists."
  ; The C version has queries = [X, Y]; queryVector = [&X, &Y];
  ; parentQuery = Z; and parentQueryVector = [&Z].
  ; In Clojure, we have queries = [X, Y, Z]; query-vector = [0, 1];
  ; parent-query-vector = [2]
  (let [; A. Find best local index
        ; A.1 find local-log-likelihood for every variable except v
        other-local-log-likelihoods
          (for [vv (range (:n-var adtree))
                :when (not= vv v)]
            (let [initial-queries
                    [{:index vv :value nil}
                     {:index vv :value nil}
                     {:index vv :value nil}]
                  queries
                    (assoc initial-queries (if (< v vv) 0 1) vv)
                  query-vector        [0 1]
                  parent-query-vector [2]
                  other-local-log-likelihood
                    (+
                      (compute-specific-local-log-likelihood adtree
                        (assoc queries 0 0 1 0 2 0)
                        query-vector parent-query-vector)
                      (compute-specific-local-log-likelihood adtree
                        (assoc queries 0 0 1 1 2 (if (< vv v) 0 1))
                        query-vector parent-query-vector)
                      (compute-specific-local-log-likelihood adtree
                        (assoc queries 0 1 1 0 2 (if (< vv v) 1 0))
                        query-vector parent-query-vector)
                      (compute-specific-local-log-likelihood adtree
                        (assoc queries 1 1 1 1 2 1)
                        query-vector parent-query-vector))]
              {:index vv :value other-local-log-likelihood}))
        ; A.2 Sort them and take last (highest)
        best-local
          (last (sort-by :value other-local-log-likelihoods))]
    (if (> (:local-log-likelihood best-local) this-local-log-likelihood)
      (let [penalty        (* -0.5 (Math/log (double (:n-record adtree))))
            log-likelihood (* (:n-record adtree)
                             (+ base-log-likelihood
                               (:value best-local)
                               (- this-local-log-likelihood)))
            score          (+ penalty log-likelihood)]
        {:op      :insert
         :from-id (:index best-local)
         :to-id   v
         :score   score})
      nil)))

(defn- create-tasks [learner i n]
  "Create tasks and add them to learner. This is thread `i` of `n`."
  (let [adtree                     (:adtree learner)
        vars                       (create-partition 0 (:n-var adtree) i n)
                                   ; subset of variables for this thread
        local-base-log-likelihoods (compute-local-base-log-likelihoods vars adtree)
        base-log-likelihood        (sum local-base-log-likelihoods)]
    (dosync
      (alter (:base-log-likelihood learner) + base-log-likelihood))
    (let [tasks (filter some?
                  (map-indexed
                    (fn [v_i v]
                      (create-task v adtree base-log-likelihood
                        (nth local-base-log-likelihoods v_i)))
                    vars))]
          ; TODO: maybe doall to force execution before tx?
      (println "say what" tasks)
      (dosync
        ; TODO: ordering of tasks
        (alter (:tasks learner) concat tasks)))))

(defn run [learner]
  "TODO"
  (let [a (for [i (range (:n-thread learner))]
            (future (create-tasks learner i (:n-thread learner))))
        _ (doall (map deref a))]
    learner))
