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
   :local-base-log-likelihoods (ref (vec (repeat (:n-var data) 0.0)))
   :base-log-likelihood        (ref 0.0)
   :tasks                      (ref []) ; sorted by compareTask
   :n-total-parent             (ref 0)
   :n-thread                   n-thread})

(defn- add-task [tasks task]
  "Add `task` to `tasks`, ordered by score."
  (dosync
    (ref-set tasks (vec (sort-by :score (conj @tasks task))))))

(defn- add-tasks [tasks new-tasks]
  "Add `new-tasks` to `tasks`, ordered by score."
  (dosync
    (ref-set tasks (vec (sort-by :score (concat @tasks new-tasks))))))

(defn- pop-task [tasks]
  "Returns first element of `tasks`, and pops that element from `tasks`. Returns
  nil if tasks is empty."
  (dosync
    (if (empty? @tasks)
      nil
      (let [v (peek @tasks)]
        (alter tasks pop)
        v))))

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
                    (assoc-in initial-queries [(if (< v vv) 0 1) :index] vv)
                  query-vector        [0 1]
                  parent-query-vector [2]
                  other-local-log-likelihood
                    (+
                      (compute-specific-local-log-likelihood adtree
                        (-> queries
                          (assoc-in [0 :value] 0)
                          (assoc-in [1 :value] 0)
                          (assoc-in [2 :value] 0))
                        query-vector parent-query-vector)
                      (compute-specific-local-log-likelihood adtree
                        (-> queries
                          (assoc-in [0 :value] 0)
                          (assoc-in [1 :value] 1)
                          (assoc-in [2 :value] (if (< vv v) 0 1)))
                        query-vector parent-query-vector)
                      (compute-specific-local-log-likelihood adtree
                        (-> queries
                          (assoc-in [0 :value] 1)
                          (assoc-in [1 :value] 0)
                          (assoc-in [2 :value] (if (< vv v) 1 0)))
                        query-vector parent-query-vector)
                      (compute-specific-local-log-likelihood adtree
                        (-> queries
                          (assoc-in [0 :value] 1)
                          (assoc-in [1 :value] 1)
                          (assoc-in [2 :value] 1))
                        query-vector parent-query-vector))]
              {:index vv :value other-local-log-likelihood}))
        ; A.2 Sort them and take last (highest)
        best-local
          (last (sort-by :value other-local-log-likelihoods))]
    (if (> (:value best-local) this-local-log-likelihood)
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
      (println "tasks created by thread" i ":" tasks)
      (add-tasks (:tasks learner) tasks))))

(defn- is-task-valid? [task net]
  (let [from (:from-id task)
        to   (:to-id task)]
    (case (:op task)
      :insert  (not (or (net/has-edge? net from to)
                        (net/is-path?  net to from)))
      :remove  true ; can never create cycle, so always valid
      :reverse (not (net/is-path?
                      (net/apply-operation net :remove from to)
                      ; temporarily remove edge for check
                      from to))
      (println "error: unknown task operation type" (:op task)))))

(defn- calculate-delta-log-likelihood [task learner]
  "Returns delta-log-likelihood, and sets the local-base-log-likelihoods and
  n-total-parent of the learner."
  (let [net    (:net learner)
        adtree (:adtree learner)
        to     (:to-id task)
        local-base-log-likelihoods (:local-base-log-likelihoods learner)]
    (case (:op task)
      :insert
        (let [queries (vec (for [v (range (:n-var adtree))]
                        {:index v :value QUERY_VALUE_WILDCARD}))]
          (dosync
            (let [[query-vector parent-query-vector]
                    (populate-query-vectors net to queries)
                  new-base-log-likelihood
                    (compute-local-log-likelihood to adtree
                      queries query-vector parent-query-vector)
                  to-local-base-log-likelihood
                    (nth @local-base-log-likelihoods to)
                  delta-log-likelihood
                    (- to-local-base-log-likelihood new-base-log-likelihood)]
              (alter local-base-log-likelihoods assoc to new-base-log-likelihood)
              ; The following happens in a separate tx in the C version
              (commute (:n-total-parent learner) inc)
              delta-log-likelihood)))
      (println "error: unknown task operation type" (:op task)))))

(defn- find-best-insert-task [learner to-id n-total-parent base-penalty base-log-likelihood]
  "TODO, returns task."
  (let [net (:net learner)
        queries (for [v (range (:n-var (:adtree learner)))]
                  {:index v :value QUERY_VALUE_WILDCARD})]
    (dosync
      (let [; Create query-vector and parent-query-vector
            ; TODO: why is this in the tx?
            parent-query-vector (populate-parent-query-vector net to-id queries)
            query-vector        (conj parent-query-vector to-id)
            ; Search all possible valid operations for better local log likelihood
            parent-id-list (net/get-parent-id-list net to-id)
            global_max-num-edge-learned 1] ; TODO: get this from somewhere
        (if (or (< global_max-num-edge-learned 0)
                (<= (count parent-id-list) global_max-num-edge-learned))
          (let [old-local-log-likelihood
                  (nth @(:local-base-log-likelihoods learner) to-id)
                invalid-bitmap
                  (net/find-descendants net to-id)
                invalid-bitmap-2
                  (reduce (fn [invalid parent-id] (bitmap/set invalid parent-id))
                    invalid-bitmap parent-id-list)
                ; TODO: maybe set instead of bitmap?
                parent-local-log-likelihoods
                  (for [from-id parent-id-list
                        :when (not (bitmap/is-set? invalid-bitmap-2 from-id))
                        :when (not= from-id to-id)]
                    (let [local-log-likelihood
                            (compute-local-log-likelihood
                              to-id
                              (:adtree learner)
                              net
                              queries
                              (sort-queries (conj query-vector (nth queries from-id)))
                              (sort-queries (conj parent-query-vector (nth queries from-id))))]
                      {:from-id from-id
                       :local-log-likelihood local-log-likelihood}))
                {best-from-id :from-id best-local-log-likelihood :local-log-likelihood}
                  (->>
                    (conj parent-local-log-likelihoods
                      {:from-id to-id :local-log-likelihood old-local-log-likelihood})
                    (sort-by :local-log-likelihood)
                    (first))
                global_insert-penalty 1 ; XXX
                score
                  (if (= best-from-id to-id)
                    0.0
                    (let [n-record (:n-record (:adtree learner))
                          n-parent (inc (count parent-id-list))
                          penalty  (* base-penalty
                                      (+ n-total-parent
                                         (* n-parent global_insert-penalty)))
                          log-likelihood (* n-record
                                            (+ base-log-likelihood
                                               best-local-log-likelihood
                                               (- old-local-log-likelihood)))]
                      (+ penalty log-likelihood)))]
            {:op      :insert
             :from-id best-from-id
             :to-id   to-id
             :score   score})
          {:op      :insert
           :from-id to-id
           :to-id   to-id
           :score   0.0})))))

(defn- learn-structure [learner i n]
  (loop []
    (let [task (pop-task (:tasks learner))]
      (when (not (nil? task))
        (let [valid?
                (atom false)
              updated-net
                (dosync ; TODO: why is this in a transaction? -> nodes in net should be refs, or contain refs
                  ; Check if task is still valid
                  (reset! valid? (is-task-valid? task (:net learner)))
                  (if @valid?
                    ; Perform task: update graph and probabilities
                    (net/apply-operation (:net learner) (:op task) (:from-id task) (:to-id task))))
              _ (println "task processed by thread" i ":" task (if @valid? "(valid)" "(invalid)"))
              delta-log-likelihood
                (if @valid?
                  (calculate-delta-log-likelihood task learner)
                  0.0)
              ; Update/read globals
              [base-log-likelihood n-total-parent]
                (dosync
                  [(alter (:base-log-likelihood learner) + delta-log-likelihood)
                   @(:n-total-parent learner)])
              ; Find next task
              n-record (:n-record (:adtree learner))
              base-penalty (* -0.5 (Math/log (double n-record)))
              base-score (+ (* n-total-parent base-penalty)
                            (* n-record base-log-likelihood))
              new-task (find-best-insert-task learner (:to-id task) n-total-parent base-penalty base-log-likelihood)
              global_operation-quality-factor 1.0 ; XXX get from somewhere
              best-task
                (if (and (not= (:from-id new-task) (:to-id new-task))
                         (> (:score new-task) (/ base-score global_operation-quality-factor)))
                  new-task
                  {:op :num :to-id -1 :from-id -1 :score base-score})]
          (when (not= (:to-id best-task) -1)
            (println "new task on thread" i ":" best-task)
            (add-task (:tasks learner) best-task)))
        (recur)))))

(defn run [learner]
  "TODO"
  (let [n-thread    (:n-thread learner)
        create-futs (map #(future (create-tasks    learner % n-thread)) (range n-thread))
        learn-futs  (map #(future (learn-structure learner % n-thread)) (range n-thread))]
    (doseq [f create-futs] (deref f))
    (doseq [f learn-futs]  (deref f))))
