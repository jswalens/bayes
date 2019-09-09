(defproject bayes "10.0.0"
  :description "Bayes benchmark from STAMP in Clojure, using transactional futures."
  :url "http://soft.vub.ac.be/~jswalens/chocola/"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/math.numeric-tower "0.0.4" :exclusions [org.clojure/clojure]]
                 [org.clojure/tools.cli "0.4.2" :exclusions [org.clojure/clojure]]
                 [com.taoensso/tufte "2.1.0" :exclusions [org.clojure/clojure]]]
  :resource-paths ["resources/chocola-2.0.0-standalone.jar"]
  :injections [(require 'chocola.core)]
  :main bayes.main
  :profiles {
    :dev {
      :plugins [[lein-cloverage "1.0.6"]]
    }
    :uberjar {
      :aot :all
    }
  })
