(ns bayes.main
  (:gen-class)
  (:require [taoensso.timbre.profiling :refer [profile]]))

(defn -main [& args]
  "Main function. `args` should be a list of command line arguments."
  (println "Hello world!")
  (shutdown-agents))

; To run manually:
;(main *command-line-args*)
