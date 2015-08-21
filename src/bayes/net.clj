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
   :parent-ids (priority-queue/create)   ; ordered list of parent ids
   :child-ids  (priority-queue/create)}) ; ordered list of child ids

(defn alloc [n]
  "Returns a net of `n` nodes."
  (vec (map alloc-node (range n))))

;
; get-parent-ids and get-child-ids
;

(defn get-parent-ids [net id]
  "Get ids of parents of node `id` in `net`."
  (:parent-ids (nth net id)))

(defn get-child-ids [net id]
  "Get ids of children of node `id` in `net`."
  (:child-ids (nth net id)))

;
; insert, remove and reverse edge
;

(defn insert-edge [net from-id to-id]
  "Returns `net` with an edge added from `from-id` to `to-id`."
  (-> net
    (update-in [to-id :parent-ids] priority-queue/add from-id)
    (update-in [from-id :child-ids] priority-queue/add to-id)))

(defn remove-edge [net from-id to-id]
  "Returns `net` with the edge from `from-id` to `to-id` removed."
  (-> net
    (update-in [to-id :parent-ids] priority-queue/remove from-id)
    (update-in [from-id :child-ids] priority-queue/remove to-id)))

(defn reverse-edge [net from-id to-id]
  "Returns `net` with the edge from `from-id` to `to-id` reversed."
  (insert-edge (remove-edge net from-id to-id) to-id from-id))

;
; has-edge? and has-path?
;

(defn has-edge? [net from-id to-id]
  "Does `net` have an edge between the nodes with ids `from-id` and `to-id`?"
  (.contains (get-parent-ids net to-id) from-id))

(defn has-path? [net from-id to-id]
  "Returns true if there is a path from `from-id` to `to-id` in `net`."
  (loop [queue   [from-id]
         visited #{}]
    (if (empty? queue)
      false
      (let [id (first queue)]
        (if (= id to-id)
          true
          (recur
            (concat (rest queue)
              (filter #(not (.contains visited %))
                (get-child-ids net id)))
            (conj visited id)))))))

;
; find-descendants
;

(defn- concat-uniq [xs ys]
  "Concat `xs` and `ys`, but do not add elements in `ys` that are already in
  `xs`."
  (concat xs (filter #(not (.contains xs %)) ys)))

(defn find-descendants [net id]
  "Returns set of descendants of the node `id` in `net`."
  (loop [descendants (into #{} (get-child-ids net id))
         queue       (into []  (get-child-ids net id))]
    (if (empty? queue)
      descendants
      (let [child-id (peek queue)]
        (if (= child-id id)
          (println "ERROR: could not find descendants: node" id
            "is a descendant of itself (net contains a cycle)")
          (recur
            (into descendants (get-child-ids net child-id))
            (concat-uniq (pop queue) (get-child-ids net child-id))))))))

;
; generate-random-edges
;

(defn generate-random-edges [net max-num-parent percent-parent]
  "Extends `net` with random edges, maximally `max-num-parent` for each node
  with a chance of `percent-parent`."
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
      [n p])))
    ; TODO: assert (not (is-cycle? net).
