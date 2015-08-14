(ns bayes.adtree
  (:require [bayes.data :as data]))

(declare make-vary)
(declare make-node)

(defn- make-vary [parent-i i start data]
  (let [data (if (and (not= (inc parent-i) i) (> (:n-record data) 1))
                (data/sort data i) ; TODO: not passing start is that ok?
                data)
        num0 (data/find-split data start i)
        num1 (- (:n-record data) num0)
        most-common-value (if (>= num0 num1) 0 1)]
    {:index             i
     :most-common-value most-common-value
     :zero-node         (if (or (= num0 0) (= most-common-value 0))
                          nil
                          (->
                            (make-node i i start num0 data)
                            (assoc :value 0)))
     :one-node          (if (or (= num1 0) (= most-common-value 1))
                          nil
                          (->
                            (make-node i i (+ start num0) num1 data)
                            (assoc :value 1)))}))

(defn- make-node [parent-i i start data]
  {:index       i
   :value       -1
   :count       (:n-record data)
   :vary-vector (for [v (range (inc i) (:n-var data))]
                  (make-vary parent-i v start data))})

(defn make [data]
  "Make ADTree (alternating decision tree) based on `data`."
  {:n-var     (:n-var data)
   :n-record  (:n-record data)
   :root-node (make-node -1 -1 0 (data/sort data 0))})