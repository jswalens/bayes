(ns bayes.net
  (:require [priority-queue]
            [bitmap]))

; Net node marks:
; :init
; :done
; :test

(defn- alloc-node [id]
  "Returns an empty node with `id`."
  {:id             id
   :parent-id-list (priority-queue/create) ; list of parent ids, ordered
   :child-id-list  (priority-queue/create) ; list of child ids, ordered
   :net-node-mark  :init})

(defn alloc [n]
  "Returns a net of `n` nodes."
  (vec (map alloc-node (range n))))

(defn get-parent-id-list [net id]
  (:parent-id-list (nth net id)))

(defn get-child-id-list [net id]
  (:child-id-list (nth net id)))

(defn- insert-edge [net from-id to-id]
  "Returns `net` with an edge added from `from-id` to `to-id`."
  (-> net
    (update-in [to-id :parent-id-list] priority-queue/add from-id)
    (update-in [from-id :child-id-list] priority-queue/add to-id)))

(defn- remove-edge [net from-id to-id]
  "Returns `net` with the edge from `from-id` to `to-id` removed."
  (-> net
    (update-in [to-id :parent-id-list] priority-queue/remove from-id)
    (update-in [from-id :child-id-list] priority-queue/remove to-id)))

(defn- reverse-edge [net from-id to-id]
  "Returns `net` with the edge from `from-id` to `to-id` reversed."
  (insert-edge (remove-edge from-id to-id) to-id from-id))

(defn apply-operation [net op from-id to-id]
  "Insert, remove, or reverse an edge of the net."
  ((case op
    :insert  insert-edge
    :remove  remove-edge
    :reverse reverse-edge)
    net from-id to-id))

(defn has-edge? [net from-id to-id]
  "Does `net` have an edge between the nodes with ids `from-id` and `to-id`?"
  (.contains (get-parent-id-list net to-id) from-id))

(defn is-path? [net from-id to-id]
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
                (get-child-id-list net id)))
            (conj visited id)))))))

(defn- concat-without-dups [xs ys]
  "Concat `xs` and `ys`, but do not add elements in `ys` that are already in
  `xs`."
  (concat xs (filter #(.contains xs %) ys)))

(defn find-descendants [net id]
  "Returns set of descendants of the node `id` in `net`."
  (loop [descendants (into #{} (get-child-id-list net id))
         queue       (into []  (get-child-id-list net id))]
    (if (empty? queue)
      descendants
      (let [child-id (peek queue)]
        (if (= child-id id)
          (println "error: could not find descendants")
          (recur
            (into descendants (get-child-id-list net child-id))
            (concat-without-dups (pop queue) (get-child-id-list net child-id))))))))

(defn generate-random-edges [net max-num-parent percent-parent]
  "Extends `net` with random edges, maximally `max-num-parent` for each node
  with a chance of `percent-parent`."
  (reduce
    (fn [net [n p]]
      (if (< (rand-int 100) percent-parent)
        (let [parent (rand-int (count net))]
          (if (and (not= parent n)
                   (not (has-edge? net parent n))
                   (not (is-path? net n parent)))
            (insert-edge net parent n)
            net))
        net))
    net
    (for [n (range (count net))
          p (range max-num-parent)]
      [n p])))
    ; TODO: assert (not (is-cycle? net). Or not?
