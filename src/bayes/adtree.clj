(ns bayes.adtree
  (:require [bayes.data :as data]))

;
; make
;

(declare make-node)
(declare make-vary)

(defn- make-node [parent-i i start n data]
  "Create a node in the adtree. A node contains a vary vector, whose elements
  have pointers to other nodes.

  Count is the number of records it contains."
  {:index       i
   :value       -1
   :count       n
   :vary-vector (doall (for [v (range (inc i) (:n-var data))]
                  (make-vary parent-i v start n data)))})

(defn- make-vary [parent-i i start n data]
  "Make an element of the vary vector of a node in the adtree."
  (let [data (if (and (not= (inc parent-i) i) (> n 1))
                (data/sort data start n i)
                data)
        n-0  (data/find-split data start n i) ; number of 0s
        n-1  (- n n-0)                        ; number of 1s
        most-common-value (if (>= n-0 n-1) 0 1)]
    {:index             i
     :most-common-value most-common-value
     :zero-node         (if (or (= n-0 0) (= most-common-value 0))
                          nil
                          (->
                            (make-node i i start n-0 data)
                            (assoc :value 0)))
     :one-node          (if (or (= n-1 0) (= most-common-value 1))
                          nil
                          (->
                            (make-node i i (+ start n-0) n-1 data)
                            (assoc :value 1)))}))

(defn make [data]
  "Make ADTree (alternating decision tree) based on `data`."
  {:n-var     (:n-var data)
   :n-record  (:n-record data)
   :root-node (make-node -1 -1 0 (:n-record data)
                (data/sort data 0 (:n-record data) 0))})

;
; get-count
;

(defn- drop-one [i coll]
  "Returns `coll` with the element at index `i` removed."
  (concat
    (take i coll)
    (drop (inc i) coll)))

(defn- swap-bit [i]
  "(swap-bit 0) = 1, (swap-bit 1) = 0"
  (- 1 i))

(declare get-count)

(defn- get-count-helper [node q queries query-vector last-query-index adtree]
  (cond
    (nil? node)
      0
    (>= (:index node) last-query-index)
      (:count node)
    (>= q (count query-vector)) ; (nth query-vector q) will fail
      (:count node)
    :else
      (let [; q is index in query-vector
            ; query-index = query-vector[q] = the index of the query in queries
            ; queries[query-index] = the query
            query-index          (nth query-vector q)
            {query-value :value} (nth queries query-index)
            vary-index           (- query-index (:index node) 1)
            vary                 (nth (:vary-vector node) vary-index)]
        (if (= query-value (:most-common-value vary))
          (let [super-query-vector
                  (drop-one q query-vector)
                super-count
                  (get-count adtree queries super-query-vector)
                inverted-queries
                  (update-in queries [query-index :value] swap-bit)
                invert-count
                  ; this call will end up in the else branch below
                  (get-count-helper node q inverted-queries query-vector
                    last-query-index adtree)
                diff
                  (- super-count invert-count)]
            (if (< diff 0)
              (do
                (println "ERROR: super count <= invert count, circumventing")
                0)
              diff))
          (case query-value
            0
              (get-count-helper (:zero-node vary) (inc q) queries query-vector
                last-query-index adtree)
            1
              (get-count-helper (:one-node vary) (inc q) queries query-vector
                last-query-index adtree)
            ; QUERY_VALUE_WILDCARD
              (throw (Exception. (str "unrecognized query value " query-value))))))))

(defn get-count [adtree queries query-vector]
  "Get count of (root node of) adtree."
  (let [last-query-index (or (last query-vector) -1)]
    (get-count-helper (:root-node adtree) 0 queries query-vector
      last-query-index adtree)))
