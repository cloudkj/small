(ns lambda-ml.decision-tree)

(defn gini-impurity
  [y]
  (let [total (count y)]
    (->> (vals (frequencies y))
         (map #(/ % total))
         (map #(* % (- 1 %)))
         (reduce +))))

(defn categorical-partitions
  "Given a seq of k distinct values, returns the 2^{k-1}-1 possible binary
  partitions of the values into sets. Assumes k > 1."
  [vals]
  (let [partition [(hash-set (first vals))
                   (set (rest vals))]]
    (if (<= (count vals) 2)
      [partition]
      (reduce (fn [p [s1 s2]]
                (conj p
                      [(conj s1 (first vals)) s2]
                      [(conj s2 (first vals)) s1]))
              [partition]
              (categorical-partitions (rest vals))))))

(defn numeric-partitions
  "Given a seq of k distinct numeric values, returns k-1 possible binary
  partitions of the values by taking the average of consecutive elements in the
  sorted seq of values."
  [vals]
  (loop [partitions []
         v (sort vals)]
    (if (< (count v) 2)
      partitions
      (recur (conj partitions (/ (+ (first v) (second v)) 2))
             (rest v)))))

(defn splitters
  "Returns a seq of all possible splitters for feature i. A splitter is a
  predicate function that evaluates to true if an example belongs in the left
  subtree, or false if an example belongs in the right subtree, based on the
  splitting criterion."
  [x i]
  (let [sample (first (map #(nth % i) x))]
    (cond (number? sample) nil ;; TODO: generate splitters for numeric features
          (string? sample) (->> (map #(nth % i) x)
                                (distinct)
                                (categorical-partitions)
                                (map (fn [[s1 s2]]
                                       (with-meta
                                         (fn [x] (contains? s1 (nth x i)))
                                         {:decision [s1 s2]}))))
          :else (throw (IllegalStateException. "Invalid feature type")))))

