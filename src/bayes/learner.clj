(ns bayes.learner
  (:require [bayes.net :as net]))

(defn alloc [data adtree]
  "Allocate the learner.

  In the C version, parts of this struct are aligned to cache lines. We don't
  care about that here."
  {:adtree                     adtree
   :net                        (net/alloc (:n-var data))
   :local-base-log-likelihoods [] ; will contain (:n-var data) floats
   :base-log-likelihood        0.0
   :tasks                      [] ; will contain (:n-var data) learner tasks
   :task-list                  (list) ; TODO: sorted by compareTask
   :n-total-parent             0})

(defn score [learner]
  0)
