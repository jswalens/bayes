(ns bayes.data
  (:require [clojure.math.numeric-tower :as math]
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
              (rand-int (inc DATA_PRECISION))))]
    ; Create variable dependency ordering for record generation
    #_(loop [order        []
           queue        []
           dependencies []
           ordered      ...
           done         ...]
      (let [v (bitmap/find-clear done (vorige v + 1))]
        (if (< v 0)
          ...break out of loop...
          (if (not= (count (net/get-child-id-list net v)) 0)
            (recur order queue dependencies ordered done)
            (let [; Use breadth-first search to find net connected to this leaf
                  [new-queue updated-done updated-dependencies]
                    (reduce
                      (fn [{q :queue don :done dep :dependencies} id]
                        {:queue
                          (reduce
                            (fn [q_ parent-id]
                              (conj q_ parent-id))
                            q
                            (net/get-parent-id-list net id))
                         :done         ...set id to true in done...
                         :dependencies (conj dep id)})
                      {:queue [] :done done :dependencies dependencies}
                      queue)
                  ; Create ordering
                  ...]
              (recur ...))))))
    ; Create records
    ; TODO
    ; Clean up
    ; TODO
    nil))
