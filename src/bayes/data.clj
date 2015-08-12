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
   ; records is a map record id -> list of variables (of length n-var) ->
   ; 0 or 1 (see generate)
   :records  []})

(defn- bits->bitmap [bits]
  "Convert a list of 0 and 1 to a bitmap, i.e. an int where each bit is set
  correspondingly.
  (bits->bitmap (list 1 0 1)) => 0b101"
  (reduce
    (fn [bitmap bit]
      (+ (* bitmap 2) bit))
    0
    bits))

(defn generate [data max-num-parent percent-parent]
  "Generate data, returns `{:data data :net net}`.

  As opposed to C++ version, this doesn't take a seed."
  (let [; Generate random Bayesian network
        net
          (net/generate-random-edges
            (net/alloc (:n-var data)) max-num-parent percent-parent)
        ; Create a threshold for each of the possible permutations of variable
        ; value instances
        ; In the C++ version, this is a 2D array variable -> bitmap -> (random)
        ; int. The bitmap has length = the variable's number of parents. All
        ; permutations of the bitmap are iterated through. So, given a variable
        ; and an on/off state for each of its parents, this returns an integer.
        thresholds
          (for [v (range (:n-var data))]
            (for [t (range (math/expt 2 (count (net/get-parent-id-list net v))))]
              (rand-int (inc DATA_PRECISION))))
        ; Create variable dependency ordering for record generation.
        ; Each of order[i]'s parents are sorted before i in order, i.e.
        ; for all i: for all p in parents[order[i]]:
        ; index-of(p in order) < i   [1]
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
                  order done)
                ; This node has no children, it is a leaf
                (let [; Use breadth-first search to find net connected to this leaf
                      [updated-done dependencies]
                        (loop [queue        [id]
                               updated-done done
                               dependencies []]
                          (if (empty? queue)
                            [updated-done dependencies]
                            (recur
                              (concat queue
                                (net/get-parent-id-list net (first queue)))
                              (bitmap/set updated-done (first queue))
                              (conj dependencies (first queue)))))
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
                    updated-order updated-done)))))
        ; Create records
        records
          (for [r (range (:n-record data))]
            (reduce
              (fn [record o]
                (let [v (nth order o)
                      values ; list of 0s and 1s
                        (for [p (net/get-parent-id-list net v)]
                          ; ordering ensures that p < o (see [1]), so record
                          ; will have an index p at iteration o of the reduce
                          (nth record p))
                      bitmap
                        (bits->bitmap values)
                      threshold
                        (get-in thresholds [v bitmap])
                      rnd
                        (rand-int DATA_PRECISION)]
                  (if (< rnd threshold)
                    1
                    0)))
              []
              (range (:n-var data))))]
    ; Return
    {:data (assoc data :records records) :net net}))
