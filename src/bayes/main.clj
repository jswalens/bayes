(ns bayes.main
  (:gen-class)
  (:refer-clojure :exclude [time])
  (:require [random]
            [bayes.options :as options]
            [bayes.data :as data]
            [bayes.adtree :as adtree]
            [bayes.net :as net]
            [bayes.learner :as learner]
            [log :refer [log time]]
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
                  _ (log "Generating data...")
                  {:keys [data net]} (p :generate-data (time (data/generate params)))
                  _ (log "done.")
                  ; Generate adtree
                  _ (log "Generating adtree...")
                  adtree (p :generate-adtree (time (adtree/make data)))
                  _ (log "done.")
                  ; Score original network
                  actual-score (p :score-original (score-original net adtree params))
                  ; Learn structure of Bayesian network
                  _ (log "Learning structure...")
                  learner (p :alloc-learner (learner/alloc adtree params))
                  _ (p :run-learner (time (learner/run learner)))
                  _ (log "done.")
                  ; Check solution
                  status (p :check-solution (net/has-cycle? (:net learner)))
                  _ (when status (log "ERROR: solution is incorrect"))
                  ; Score learner network
                  learn-score (p :score-solution (learner/score learner))]
              (log "Learn score  =" learn-score)
              (log "Actual score =" actual-score)))]
    (when (:profile params) (log (format-pstats pstats)))))

(defn -main [& args]
  "Main function. `args` should be a list of command line arguments."
  (time (main args))
  ; Eliminate one minute wait (see doc shutdown-agents)
  (await log/logger)
  (shutdown-agents))

; To run manually:
;(main *command-line-args*)
