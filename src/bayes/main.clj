(ns bayes.main
  (:gen-class)
  (:require [random]
            [bayes.data :as data]
            [bayes.adtree :as adtree]
            [bayes.learner :as learner]
            [taoensso.timbre.profiling :refer [profile p defnp]]))

(def default-params
  {:edge    -1   ; -1 means no limit
   :insert  1
   :number  4
   :percent 10
   :quality 1.0
   ; In the C version, the default for :record is 4096 and for :var 32. However,
   ; adtree/make takes > 500 seconds on my machine for these values. For 256
   ; records and 16 variables this is only 0.4s.
   :record  256  ; 4096
   :seed    1
   :thread  1
   :var     16}) ; 32

(def usage
"Usage: ./bayes [options]

Options:                                         (defaults)

    e <UINT>   Max [e]dges learned per variable  (-1)
    i <UINT>   Edge [i]nsert penalty             (1)
    n <UINT>   Max [n]umber of parents           (4)
    p <UINT>   [p]ercent chance of parent        (10)
    q <FLT>    Operation [q]uality factor        (1.0)
    r <UINT>   Number of [r]ecords               (4096)
    s <UINT>   Random [s]eed                     (1)
    t <UINT>   Number of [t]hreads               (1)
    v <UINT>   Number of [v]ariables             (32)")

(defn parse-args [args]
  "Parse the arguments."
  ; TODO: actually parse arguments
  default-params)

(defnp score [net adtree params]
  "Score `net` without learning."
  (let [learner (assoc (learner/alloc adtree params) :net net)]
    (learner/score learner)))

(defnp -main [& args]
  "Main function. `args` should be a list of command line arguments."
  (profile :info :all
    ; Initialization
    (let [params (p :parse-args (parse-args args))]
      (println "Random seed                =" (:seed params))
      (println "Number of vars             =" (:var params))
      (println "Number of records          =" (:record params))
      (println "Max num parents            =" (:number params))
      (println "% chance of parent         =" (:percent params))
      (println "Insert penalty             =" (:insert params))
      (println "Max num edge learned / var =" (:edge params))
      (println "Operation quality factor   =" (:quality params))
      ; Generate data
      (println "Generating data...")
      (random/set-seed (:seed params))
      (let [{data :data net :net} (p :generate-data (data/generate params))
            _ (println "done.")
            ; Generate adtree
            _ (println "Generating adtree...")
            adtree (p :generate-adtree (time (adtree/make data)))
            _ (println "done.")
            ; Score original network
            actual-score (p :score-original (score net adtree params))
            _ (println "actual score:" actual-score)
            ; Learn structure of Bayesian network
            _ (println "Learning structure...")
            learner (p :alloc-learner (learner/alloc adtree params))
            _ (p :run-learner (time (learner/run learner)))
            _ (println "done.")
            ; Check solution
            ; TODO implement net/is-cycle? to check solution (optional)
            ;status (net/is-cycle? (:net learner))
            ;_ (when-not status (println "ERROR: solution is incorrect"))
            learn-score (p :score-solution (learner/score learner))
            _ (println "Learn score  =" learn-score)
            _ (println "Actual score =" actual-score)]
        nil)))
  ; Clean up
  ; shutdown-agents should be after profile, else RejectedExecutionException is
  ; raised in timbre
  (shutdown-agents))

; To run manually:
;(main *command-line-args*)
