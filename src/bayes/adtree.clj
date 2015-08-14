(ns bayes.adtree
  (:require [bayes.data :as data]))

(declare make-vary)
(declare make-node)

(defn- make-vary [parent-i i start n data]
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

(defn- make-node [parent-i i start n data]
  {:index       i
   :value       -1
   :count       n
   :vary-vector (doall (for [v (range (inc i) (:n-var data))]
                  (make-vary parent-i v start n data)))})

(defn make [data]
  "Make ADTree (alternating decision tree) based on `data`."
  {:n-var     (:n-var data)
   :n-record  (:n-record data)
   :root-node (make-node -1 -1 0 (:n-record data)
                (data/sort data 0 (:n-record data) 0))})

(defn get-count [adtree query-vector]
  "TODO"
  0)
