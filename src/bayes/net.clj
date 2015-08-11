(ns bayes.net)

; Net node marks:
; :init
; :done
; :test

(defn- alloc-node [id]
  {:id             id
   :parent-id-list [] ; TODO: ordered by comparing pointers
   :child-id-list  [] ; TODO: ordered by comparing pointers
   :net-node-mark  :init})

(defn alloc [n-node]
  (map alloc-node (range n-node)))

(defn has-edge? [net from-id to-id]
  "Does `net` have an edge between the nodes with ids `from-id` and `to-id`?"
  (.contains (:parent-id-list (nth net to-id)) from-id))

;(defn is-path? [net n parent] TODO)

;(defn- insert-edge [net parent n] TODO)

(defn generate-random-edges [net max-num-parent percent-parent]
  (for [n (range (count net))
        p (range max-num-parent)]
    (if (< (rand-int 100) percent-parent)
      (let [parent (rand-int (count net))]
        (if (and (not= parent n)
                 (not (has-edge? net parent n))
                 (not (is-path? net n parent)))
          (insert-edge net parent n) ; TODO: returns new net...
          ))))
  ; TODO: assert (not (is-cycle? net)
  )
