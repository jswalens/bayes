(ns bayes.data
  (:require [bayes.net :as net]))

(defn alloc [n-var n-record]
  "Allocate data structure.

  In C++, this allocates :records to the right length, in Clojure we don't care."
  {:n-var    n-var
   :n-record n-record
   :records  []}) ; will eventually have length n-var * n-record

(defn generate [data max-num-parent percent-parent]
  "Generate data.

  Compared to C++ version, doesn't take a seed."
  ; Generate random Bayesian network
  (let [net (net/alloc (:n-var data))]
    (net/generate-random-edges net max-num-parent percent-parent))
  ; TODO
  ; Create a threshold for each of the possible permutation of variable value instances
  ; TODO
  ; Create variable dependency ordering for record generation
  ; TODO
  ; Create records
  ; TODO
  ; Clean up
  ; TODO
  nil)
