(defproject bayes "10.0.0"
  :description "Bayes benchmark from STAMP in Clojure, using transactional futures."
  :url "http://soft.vub.ac.be/~jswalens/chocola/"
  :dependencies [[org.clojure/math.numeric-tower "0.0.4"]
                 [org.clojure/tools.cli "0.3.3"]
                 [com.taoensso/timbre "4.1.4"]]
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
