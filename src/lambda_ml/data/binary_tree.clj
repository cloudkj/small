(ns lambda-ml.data.binary-tree)

(defn make-tree
  ([val]
   (make-tree val nil nil))
  ([val left right]
   (vector val left right)))

(defn get-value
  [tree]
  (nth tree 0))

(defn get-left
  [tree]
  (nth tree 1))

(defn get-right
  [tree]
  (nth tree 2))

(defn get-path
  [tree paths]
  (->> paths
       (map (fn [path]
              (cond (= path :left)  1
                    (= path :right) 2
                    :else (throw (IllegalArgumentException. "Invalid tree path")))))
       (get-in tree)))

(defn leaf?
  [tree]
  (and (nil? (get-left tree)) (nil? (get-right tree))))

(defn print-tree
  ([tree]
   (print-tree tree 0))
  ([tree level]
   (println (str (apply str (repeat level "    "))
                 (let [val (get-value tree)]
                   (or (meta val) val))))
   (when (not (nil? (get-left tree)))
     (print-tree (get-left tree) (inc level)))
   (when (not (nil? (get-right tree)))
     (print-tree (get-right tree) (inc level)))))

(defn adjacency-matrix
  "Returns an adjacency matrix representation of a binary tree."
  ([tree]
   (adjacency-matrix tree {}))
  ([tree matrix]
   (let [left   (get-left tree)
         matrix (if (nil? left) matrix (adjacency-matrix left matrix))
         edges  (if (nil? left) [] [(dec (count matrix))])
         right  (get-right tree)
         matrix (if (nil? right) matrix (adjacency-matrix right matrix))
         edges  (if (nil? right) edges (conj edges (dec (count matrix))))]
     (assoc matrix (count matrix) {:edges edges :value (get-value tree)}))))
