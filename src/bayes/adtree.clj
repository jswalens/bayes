(ns bayes.adtree
  (:require [bayes.data :as data]))

(declare make-vary)
(declare make-node)

(defn- make-vary [parent-i i start n data]
  (let [data (if (and (not= (inc parent-i) i) (> n 1))
                (data/sort data start n i)
                data)
        n-0  (data/find-split data start n i) ; number of 0s
        n-1  (- n n-0)                        ; number of 1s
        most-common-value (if (>= n-0 n-1) 0 1)]
    {:index             i
     :most-common-value most-common-value
     :zero-node         (if (or (= n-0 0) (= most-common-value 0))
                          nil
                          (->
                            (make-node i i start n-0 data)
                            (assoc :value 0)))
     :one-node          (if (or (= n-1 0) (= most-common-value 1))
                          nil
                          (->
                            (make-node i i (+ start n-0) n-1 data)
                            (assoc :value 1)))}))

(defn- make-node [parent-i i start n data]
  {:index       i
   :value       -1
   :count       n
   :vary-vector (doall (for [v (range (inc i) (:n-var data))]
                  (make-vary parent-i v start n data)))})

(defn make [data]
  "Make ADTree (alternating decision tree) based on `data`."
  {:n-var     (:n-var data)
   :n-record  (:n-record data)
   :root-node (make-node -1 -1 0 (:n-record data)
                (data/sort data 0 (:n-record data) 0))})

(defn- drop-one [i coll]
  "Returns `coll` with the element at index `i` removed."
  (concat
    (take i coll)
    (drop (inc i) coll)))

(defn- swap-bit [i]
  "(swap-bit 0) = 1, (swap-bit 1) = 0"
  (- 1 i))

(declare get-count)

(defn- get-count-helper [node i q query-vector last-query-index adtree]
  (cond
    (nil? node)
      0
    (>= (:index node) last-query-index)
      (:count node)
    (>= q (count query-vector))
      (:count node)
    :else
      (let [{query-index :index query-value :value}
              (nth query-vector q)
            vary (nth (:vary-vector node) (- query-index (:index node) 1))]
        (condp = query-value
          (:most-common-value vary)
            (let [super-query-vector
                    (drop-one q query-vector)
                  super-count
                    (get-count adtree super-query-vector)
                  inverted-query-vector
                    (update-in query-vector [q :value] swap-bit)
                  invert-count
                    (get-count-helper node i q inverted-query-vector last-query-index adtree)]
              (- super-count invert-count))
          0
            (get-count-helper (:zero-node vary) (inc i) (inc q) query-vector
              last-query-index adtree)
          1
            (get-count-helper (:one-node vary) (inc i) (inc q) query-vector
              last-query-index adtree)
          ; QUERY_VALUE_WILDCARD
            (throw (Exception. (str "unrecognized query value " query-value)))))))

(defn get-count [adtree query-vector]
  "TODO"
  (let [last-query-index
          (if (empty? query-vector)
            -1
            (:index (last query-vector)))]
    (get-count-helper (:root-node adtree) -1 0 query-vector last-query-index
      adtree)))
