(ns bayes.main
  (:gen-class)
  (:require [bayes.data :as data]
            [taoensso.timbre.profiling :refer [profile]]))

(def default-params
  {:edge    -1
   :insert  1
   :number  4
   :percent 10
   :quality 1.0
   :record  4096
   :seed    1
   :thread  1
   :var     32})

(def usage
"Usage: ./bayes [options]

Options:                                         (defaults)

    e <UINT>   Max [e]dges learned per variable  (1)
    i <UINT>   Edge [i]nsert penalty             (1)
    n <UINT>   Max [n]umber of parents           (4)
    p <UINT>   [p]ercent chance of parent        (10)
    q <FLT>    Operation [q]uality factor        (1.0)
    r <UINT>   Number of [r]ecords               (4096)
    s <UINT>   Random [s]eed (IGNORED)           (1)
    t <UINT>   Number of [t]hreads               (1)
    v <UINT>   Number of [v]ariables             (32)
")

(def log println)

(defn parse-args [args]
  "Parse the arguments."
  ; TODO: actually parse arguments
  default-params)

(defn -main [& args]
  "Main function. `args` should be a list of command line arguments."
  ; Initialization
  (let [params (parse-args args)]
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
    (let [data (data/alloc (:var params) (:record params))
          net  (data/generate data (:number params) (:percent params))]
      (println "done.")
      ; Generate adtree
      ; TODO
      ; Score original network
      ; TODO
      ; Learn structure of Bayesian network
      ; TODO
      ; Check solution
      ; TODO
      ; Clean up
      (shutdown-agents))))

; To run manually:
;(main *command-line-args*)
