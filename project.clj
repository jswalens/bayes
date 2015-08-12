(defproject bayes "0.1.0-SNAPSHOT"
  :description "STAMP Bayes in Clojure"
  :url "http://example.com/TODO"
  :dependencies [[org.clojure/math.numeric-tower "0.0.4"]
                 [com.taoensso/timbre "3.4.0"]]
  :resource-paths ["resources/clojure-1.6.0.jar"]
  :main ^:skip-aot bayes.main
  :target-path "target/%s"
  :profiles {:uberjar {:aot :all}})
