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
            (for [t (range (math/expt 2 (count (get-parent-id-list net v))))]
              (rand-int (inc DATA_PRECISION))))]
    ; Create variable dependency ordering for record generation
    ; TODO
    ; Create records
    ; TODO
    ; Clean up
    ; TODO
    nil))
