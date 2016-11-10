(ns lambda-ml.decision-tree)

(defn gini-impurity
  [y]
  (let [total (count y)]
    (->> (vals (frequencies y))
         (map #(/ % total))
         (map #(* % (- 1 %)))
         (reduce +))))

(defn numeric-partitions
  [vals]
  (loop [partitions []
         v vals]
    (if (< (count v) 2)
      partitions
      (recur (conj partitions (/ (+ (first v) (second v)) 2))
             (rest v)))))
