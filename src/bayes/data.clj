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
   ; records is a list mapping each record id to a record, which is a list of 0s
   ; and 1s of length n-var (see generate)
   :records  []})

(defn- get-record [data id]
  "Get record with index `id` from `data`."
  (nth (:records data) id))

(defn- get-var [record offset]
  "Get column `offset` in `record`.."
  (nth record offset))

(defn- get-record-var [data id offset]
  "Get column `offset` in record with index `id` from `data`."
  (get-var (get-record data id) offset))

(defn- bits->bitmap [bits]
  "Convert a list of 0 and 1 to a bitmap, i.e. an int where each bit is set
  correspondingly.
  (bits->bitmap (list 1 0 1)) => 0b101"
  (reduce
    (fn [bitmap bit]
      (+ (* bitmap 2) bit))
    0
    bits))

(defn- conj-uniq [coll x]
  "Conjoins `x` to `coll`, as conj, but only if `coll` doesn't contain x."
  (if (.contains coll x)
    coll
    (conj coll x)))

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
                            (let [[fst & rst] queue]
                              (recur
                                (concat rst (net/get-parent-id-list net fst))
                                (bitmap/set updated-done fst)
                                (conj dependencies fst)))))
                      ; Create ordering
                      updated-order
                        (reduce conj-uniq order (reverse dependencies))]
                  (recur (bitmap/find-clear done (inc id))
                    updated-order updated-done)))))
        ; Create records
        records
          (for [r (range (:n-record data))]
            (reduce
              (fn [record o]
                (let [id (nth order o)
                      values ; list of 0s and 1s
                        (for [p (net/get-parent-id-list net id)]
                          ; ordering ensures that p < o (see [1]), so record
                          ; will have an index p at iteration o of the reduce
                          (get-var record p))
                      bitmap
                        (bits->bitmap values)
                      threshold
                        (nth (nth thresholds id) bitmap)
                      rnd
                        (rand-int DATA_PRECISION)]
                  (if (< rnd threshold)
                    (assoc record id 1)
                    (assoc record id 0))))
              (vec (repeat (:n-var data) 0))
              (range (:n-var data))))]
    ; Return
    {:data (assoc data :records records) :net net}))

(defn- compare-record [a b offset]
  "Compare records `a` and `b` by a lexicographic order on its columns starting
  at `offset`.
  Assumes `a` and `b` are the same size."
  (let [c (compare (get-var a offset) (get-var b offset))]
    (if (= c 0)
      (if (>= (inc offset) (count a))
        0
        (compare-record a b (inc offset)))
      c)))

(defn sort-records [data offset]
  "Sort records in `data`, based on values in column `offset` and afterwards.
  Returns sorted records."
  (clojure.core/sort compare-record (:records data)))

(defn sort [data offset]
  "Sort records in `data`, based on values in column `offset` and afterwards.
  Returns updated data."
  (assoc data :records (sort-records data offset)))

(defn find-split [data start offset]
  "We look at the column `offset` in each record in `data`. The first `x` should
  be 0, the next `n - x` should be 1. [*] This returns `x`.

  [*] To satisfy this condition, run data/sort on the data first.

  This function uses binary search."
  (loop [low  start
         high (+ start (:n-record data) -1)]
    (if (> low high)
      (- low start)
      (let [mid (int (/ (+ low high) 2))]
        (if (= (get-record-var data mid offset) 0)
          (recur (inc mid) high)
          (recur low (dec mid)))))))
