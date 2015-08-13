(ns priority-queue)

(defn create []
  "Create an empty priority queue. Its elements will be ordered based on their
  value."
  [])

(defn add [queue val]
  "Add `val` to `queue`."
  (sort (conj queue val)))