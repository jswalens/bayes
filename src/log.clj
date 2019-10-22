(ns log
  (:refer-clojure :exclude [time]))

(def logger (agent nil))

(defn log [& msgs]
  (send logger (fn [_] (apply println msgs)))
  nil)

(defmacro time [expr]
  "Based on Clojure's time, but prints via logger."
  `(let [start# (. System (nanoTime))
         ret#   ~expr
         time#  (/ (double (- (. System (nanoTime)) start#)) 1000000.0)]
     (log (str "Elapsed time: " time# " msecs"))
     ret#))
