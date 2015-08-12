(ns bayes.net)

; Net node marks:
; :init
; :done
; :test

(defn- alloc-node [id]
  "Returns an empty node with `id`."
  {:id             id
   :parent-id-list [] ; list of parent ids; TODO: ordered by comparing pointers
   :child-id-list  [] ; list of child ids; TODO: ordered by comparing pointers
   :net-node-mark  :init})

(defn alloc [n]
  "Returns a net of `n` nodes."
  (map alloc-node (range n)))

(defn has-edge? [net from-id to-id]
  "Does `net` have an edge between the nodes with ids `from-id` and `to-id`?"
  (.contains (:parent-id-list (nth net to-id)) from-id))

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
            (reduce ; add children to queue if not visited
              (fn [q i]
                (if (not (.contains visited i))
                  (conj q i)
                  q))
              queue
              (:child-id-list (nth net id)))
            (conj visited id)))))))

(defn get-parent-id-list [net id]
  (:parent-id-list (nth net id)))

(defn get-child-id-list [net id]
  (:child-id-list (nth net id)))

(defn- insert-edge [net from-id to-id]
  "Returns `net` with an edge added from `from-id` to `to-id`."
  (-> net
    (update-in [to-id :parent-id-list] conj from-id)
    (update-in [from-id :child-id-list] conj to-id)))

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
            (insert-edge net parent n)))))
    net
    (for [n (range (count net))
          p (range max-num-parent)]
      [n p])))
    ; TODO: assert (not (is-cycle? net). Or not?
