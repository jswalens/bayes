(ns bayes.main
  (:gen-class)
  (:require [clojure.tools.cli]
            [clojure.string]
            [random]
            [bayes.data :as data]
            [bayes.adtree :as adtree]
            [bayes.learner :as learner]
            [taoensso.timbre :as timbre]
            [taoensso.timbre.profiling :refer [profile p]]))

; Variations to enable.
(def variations (atom []))

(def cli-params
  [["-e" "--edge UINT"    "Max [e]dges learned per variable (-1 for no limit)"
    :default -1
    :parse-fn #(Integer/parseInt %)]
   ["-i" "--insert UINT"  "Edge [i]nsert penalty"
    :default 1
    :parse-fn #(Integer/parseInt %)]
   ["-n" "--number UINT"  "Max [n]umber of parents"
    :default 4
    :parse-fn #(Integer/parseInt %)]
   ["-p" "--percent UINT" "[p]ercent chance of parent"
    :default 10
    :parse-fn #(Integer/parseInt %)]
   ["-q" "--quality FLT"  "Operation [q]uality factor"
    :default 1.0
    :parse-fn #(Double/parseDouble %)]
   ["-r" "--record UINT"  "Number of [r]ecords"
    :default 256
    :parse-fn #(Integer/parseInt %)]
   ["-s" "--seed UINT"    "Random [s]eed"
    :default 1
    :parse-fn #(Integer/parseInt %)]
   ["-t" "--thread UINT"  "Number of [t]hreads"
    :default 1
    :parse-fn #(Integer/parseInt %)]
   ["-v" "--var UINT"     "Number of [v]ariables"
    :default 16
    :parse-fn #(Integer/parseInt %)]
   ["-h" "--help"]
   [nil  "--variations VARIATIONS" "Comma-separated list of variations to enable."
    :default  []
    :parse-fn #(map keyword (clojure.string/split % #","))]
   [nil  "--profile"      "Enable profiling"
    :default false]])

(def c-params
  ; Default parameters of C version
  ; Takes a long time
  {:edge    -1   ; -1 means no limit
   :insert  1
   :number  4
   :percent 10
   :quality 1.0
   :record  4096
   :seed    1
   :thread  1
   :var     32})

(def fast-params
  ; Used for testing
  ; In the C version, the default for :record is 4096 and for :var 32. However,
  ; adtree/make takes > 500 seconds on my machine for these values. For 256
  ; records and 16 variables this is only 0.4s.
  {:edge    -1   ; -1 means no limit
   :insert  1
   :number  4
   :percent 10
   :quality 1.0
   :record  256
   :seed    1
   :thread  1
   :var     16})

(def paper-normal
  ; "bayes" benchmark in STAMP paper
  {:edge    2
   :insert  2
   :number  2
   :percent 20
   :quality 1.0
   :record  1024
   :seed    1
   :thread  1
   :var     32})

(def paper-larger
  ; "bayes+" benchmark in STAMP paper
  {:edge    2
   :insert  2
   :number  2
   :percent 20
   :quality 1.0
   :record  4096
   :seed    1
   :thread  1
   :var     32})

(def paper-largest
  ; "bayes++" benchmark in STAMP paper
  ; Takes a really long time
  {:edge    8
   :insert  2
   :number  10
   :percent 40
   :quality 1.0
   :record  4096
   :seed    1
   :thread  1
   :var     32})

(def usage
  (str
"Usage: ./bayes [options]

Options:

" (:summary (clojure.tools.cli/parse-opts nil cli-params))))

(defn parse-args [args]
  "Parse the command line arguments.

  Ignores errors."
  (let [{:keys [options errors]} (clojure.tools.cli/parse-opts args cli-params)]
    (when-not (empty? errors)
      (println "ERROR: Error when parsing command line arguments: "
        errors))
    options))

(defn score-original [net adtree params]
  "Score `net` without learning."
  (let [learner (assoc (learner/alloc adtree params) :net net)]
    (learner/score learner)))

(defn main [args]
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
    (println "Variations                 =" (:variations params))
    (println "Profiling?                 =" (:profile params))
    (random/set-seed (:seed params))
    (reset! variations (:variations params))
    (timbre/set-level! (if (:profile params) :trace :error))
    ; Generate data
    (println "Generating data...")
    (profile :trace :all
      (let [{data :data net :net}
              (p :generate-data (time (data/generate params)))
            _ (println "done.")
            ; Generate adtree
            _ (println "Generating adtree...")
            adtree (p :generate-adtree (time (adtree/make data)))
            _ (println "done.")
            ; Score original network
            actual-score (p :score-original (score-original net adtree params))
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
  ; Eliminate one minute wait (see doc shutdown-agents)
  ; shutdown-agents should be after profile, else RejectedExecutionException is
  ; raised in timbre
  (shutdown-agents))

(defn -main [& args]
  "Main function. `args` should be a list of command line arguments."
  (time (main args)))

; To run manually:
;(main *command-line-args*)
