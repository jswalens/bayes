(ns bayes.main
  (:gen-class)
  (:require [random]
            [bayes.options :as options]
            [bayes.data :as data]
            [bayes.adtree :as adtree]
            [bayes.net :as net]
            [bayes.learner :as learner]
            [taoensso.tufte :as tufte :refer [profiled p format-pstats]]))

(defn score-original [net adtree params]
  "Score `net` without learning."
  (let [learner (assoc (learner/alloc adtree params) :net net)]
    (learner/score learner)))

(defn main [args]
  "Main function. `args` should be a list of command line arguments."
  ; Initialization
  (let [params (options/set-args args)
        [_result pstats]
          (profiled {:level 3 :dynamic? true}
            (let [; Generate data
                  _ (println "Generating data...")
                  {:keys [data net]} (p :generate-data (time (data/generate params)))
                  _ (println "done.")
                  ; Generate adtree
                  _ (println "Generating adtree...")
                  adtree (p :generate-adtree (time (adtree/make data)))
                  _ (println "done.")
                  ; Score original network
                  actual-score (p :score-original (score-original net adtree params))
                  ; Learn structure of Bayesian network
                  _ (println "Learning structure...")
                  learner (p :alloc-learner (learner/alloc adtree params))
                  _ (p :run-learner (time (learner/run learner)))
                  _ (println "done.")
                  ; Check solution
                  status (p :check-solution (net/has-cycle? (:net learner)))
                  _ (when status (println "ERROR: solution is incorrect"))
                  ; Score learner network
                  learn-score (p :score-solution (learner/score learner))]
              (println "Learn score  =" learn-score)
              (println "Actual score =" actual-score)))]
    (when (:profile params) (println (format-pstats pstats)))
    ; Eliminate one minute wait (see doc shutdown-agents)
    ; shutdown-agents should be after profile, else RejectedExecutionException is
    ; raised in tufte
    ;(Thread/sleep 100)
    (shutdown-agents)))

(defn -main [& args]
  "Main function. `args` should be a list of command line arguments."
  (time (main args)))

; To run manually:
;(main *command-line-args*)
