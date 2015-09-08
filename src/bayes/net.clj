(ns bayes.net
  (:require [priority-queue]
            [bitmap]
            [random]))

;
; alloc
;

(defn- alloc-node [id]
  "Returns an empty node with `id`."
  {:id         id
   :parent-ids (ref (priority-queue/create))   ; ordered list of parent ids
   :child-ids  (ref (priority-queue/create))}) ; ordered list of child ids

(defn alloc [n]
  "Returns a net of `n` nodes."
  (vec (map alloc-node (range n))))

;
; get-parent-ids and get-child-ids
;

(defn get-parent-ids [net id]
  "Get ids of parents of node `id` in `net`."
  ; TODO: Should the @ be here, or in the caller of this function (to show it
  ; should be in a transaction)
  (:parent-ids (nth net id)))

(defn get-child-ids [net id]
  "Get ids of children of node `id` in `net`."
  (:child-ids (nth net id)))

;
; insert, remove and reverse edge
;

(defn insert-edge [net from-id to-id]
  "Adds an edge from `from-id` to `to-id` in `net`."
  (dosync
    (alter (get-parent-ids net to-id) priority-queue/add from-id)
    (alter (get-child-ids net from-id) priority-queue/add to-id)
    net))

(defn remove-edge [net from-id to-id]
  "Removes the edge from `from-id` to `to-id` in `net`."
  (dosync
    (alter (get-parent-ids net to-id) priority-queue/remove from-id)
    (alter (get-child-ids net from-id) priority-queue/remove to-id)
    net))

(defn reverse-edge [net from-id to-id]
  "Reverses the edge from `from-id` to `to-id` in `net`."
  (dosync
    (remove-edge net from-id to-id)
    (insert-edge net to-id from-id)
    net))

;
; has-edge?, has-path?, has-cycle?
;

(defn has-edge? [net from-id to-id]
  "Does `net` have an edge between the nodes with ids `from-id` and `to-id`?"
  (dosync
    (.contains @(get-parent-ids net to-id) from-id)))

(defn has-path? [net from-id to-id]
  "Returns true if there is a path from `from-id` to `to-id` in `net`."
  (dosync
    (loop [queue   [from-id]
           visited #{}]
      (if (empty? queue)
        false
        (let [id (first queue)]
          (if (= id to-id)
            true
            (recur
              (concat
                (rest queue)
                (filter #(not (.contains visited %)) @(get-child-ids net id)))
              (conj visited id))))))))

(defn node-in-cycle? [net id]
  "Is the node `id` part of a cycle in `net`?"
  (dosync
    (loop [to-visit (into []  @(get-child-ids net id))
           visited  (into #{} @(get-child-ids net id))]
           ; note: don't add id to visited, so we can revisit it and detect the
           ; cycle
      (if (empty? to-visit)
        false
        (let [[fst & rst] to-visit]
          (if (= fst id)
            true ; reached self
            (recur
              (concat
                rst
                (filter #(not (.contains visited %)) @(get-child-ids net fst)))
              (conj visited fst))))))))

(defn has-cycle? [net]
  "Does `net` contain any cycle? This should never be the case."
  (dosync
    (reduce #(or %1 %2) (map #(node-in-cycle? net %) (range (count net))))))

;
; find-descendants
;

(defn- concat-uniq [xs ys]
  "Concat `xs` and `ys`, but do not add elements in `ys` that are already in
  `xs`."
  (if (empty? xs)
    ys
    (concat xs (filter #(not (.contains xs %)) ys))))

(defn find-descendants [net id]
  "Returns set of descendants of the node `id` in `net`."
  (dosync
    (loop [descendants (into #{} @(get-child-ids net id))
           queue       (into []  @(get-child-ids net id))]
      (if (empty? queue)
        descendants
        (let [child-id (peek queue)]
          (if (= child-id id)
            (println "ERROR: could not find descendants: node" id
              "is a descendant of itself (net contains a cycle)")
            (recur
              (into descendants @(get-child-ids net child-id))
              (vec (concat-uniq (pop queue) @(get-child-ids net child-id))))))))))

;
; generate-random-edges
;

(defn generate-random-edges [net max-num-parent percent-parent]
  "Extends `net` with random edges, maximally `max-num-parent` for each node
  with a chance of `percent-parent`."
  ; TODO: now that net contains refs, this could use a for instead of reduce, no?
  (dosync
    (reduce
      (fn [net [n p]]
        (if (< (random/rand-int 100) percent-parent)
          (let [parent (random/rand-int (count net))]
            (if (and (not= parent n)
                     (not (has-edge? net parent n))
                     (not (has-path? net n parent)))
              (insert-edge net parent n)
              net))
          net))
      net
      (for [n (range (count net))
            p (range max-num-parent)]
        [n p]))))
      ; TODO: assert (not (is-cycle? net).
