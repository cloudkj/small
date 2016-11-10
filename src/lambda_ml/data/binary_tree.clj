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
   (println (apply str (repeat level "\t"))
            (let [val (get-value tree)]
              (or (meta val) val)))
   (when (not (nil? (get-left tree)))
     (print-tree (get-left tree) (inc level)))
   (when (not (nil? (get-right tree)))
     (print-tree (get-right tree) (inc level)))))
