(ns bayes.net
  (:require [priority-queue]))

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

(defn apply-operation [net op from-id to-id]
  "TODO"
  net)

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

(defn- insert-edge [net from-id to-id]
  "Returns `net` with an edge added from `from-id` to `to-id`."
  (-> net
    (update-in [to-id :parent-id-list] priority-queue/add from-id)
    (update-in [from-id :child-id-list] priority-queue/add to-id)))

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
