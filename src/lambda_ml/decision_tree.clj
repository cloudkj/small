(ns lambda-ml.decision-tree)

(defn gini-impurity
  [y]
  (let [total (count y)]
    (->> (vals (frequencies y))
         (map #(/ % total))
         (map #(* % (- 1 %)))
         (reduce +))))

(defn binary-partitions
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
              (binary-partitions (rest vals))))))

(defn numeric-partitions
  [vals]
  (loop [partitions []
         v vals]
    (if (< (count v) 2)
      partitions
      (recur (conj partitions (/ (+ (first v) (second v)) 2))
             (rest v)))))
