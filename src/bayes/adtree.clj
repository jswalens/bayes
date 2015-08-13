(ns bayes.adtree
  (:require [bayes.data :as data]))

(defn- make-node [parent-i i start n-record data]
  ""
  nil)

(defn make [data]
  "Make ADTree (alternating decision tree) based on `data`."
  {:n-var     (:n-var data)
   :n-record  (:n-record data)
   :root-node (make-node -1 -1 0 (:n-record data) (data/sort data 0))})
