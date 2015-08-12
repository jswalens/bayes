(ns bayes.data
  (:require [clojure.math.numeric-tower :as math]
            [bitmap]
            [bayes.net :as net]))

(def ^:const DATA_PRECISION 100)

(defn alloc [n-var n-record]
  "Allocate data structure.

  In C++, this allocates :records to the right length, in Clojure we don't care."
  {:n-var    n-var
   :n-record n-record
   :records  []}) ; will eventually have length n-var * n-record

(defn generate [data max-num-parent percent-parent]
  "Generate data.

  Compared to C++ version, doesn't take a seed."
  (let [; Generate random Bayesian network
        net
          (net/generate-random-edges
            (net/alloc (:n-var data)) max-num-parent percent-parent)
        ; Create a threshold for each of the possible permutations of variable
        ; value instances
        thresholds
          (for [v (range (:n-var data))]
            (for [t (range (math/expt 2 (count (net/get-parent-id-list net v))))]
              (rand-int (inc DATA_PRECISION))))
        ; Create variable dependency ordering for record generation
        order
          (loop [; id of node currently being visited
                 id      0
                 ; order of node ids
                 order   []
                 ; nodes that have been visited
                 done    (bitmap/create (:n-var data))]
            (if (nil? id)
              order ; bitmap/find-clear found no more nodes
              (if (not= (count (net/get-child-id-list net id)) 0)
                ; This node has children
                (recur
                  (bitmap/find-clear done (inc id))
                  order dependencies done)
                ; This node has no children
                (let [; Use breadth-first search to find net connected to this leaf
                      [updated-done dependencies]
                        (loop [queue        [id]
                               updated-done done
                               dependencies []]
                          (recur
                            (reduce
                              (fn [q parent-id]
                                (conj q parent-id))
                              queue
                              (net/get-parent-id-list net (first queue)))
                            (bitmap/set updated-done (first queue))
                            (conj dependencies (first queue))))
                      ; Create ordering
                      updated-order
                        (reduce
                          (fn [order_ dep]
                            (if (not (.contains order_ dep))
                              (conj order dep)
                              order_))
                          order
                          dependencies)]
                  (recur (bitmap/find-clear done (inc id))
                    updated-order updated-done))))))]
    ; Create records
    ; TODO
    ; Clean up
    ; TODO
    nil))
