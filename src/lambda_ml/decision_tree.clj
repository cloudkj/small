(ns lambda-ml.decision-tree
  (:require [lambda-ml.data.binary-tree :as bt]))

(defn gini-impurity
  [y]
  (let [total (count y)]
    (->> (vals (frequencies y))
         (map #(/ % total))
         (map #(* % (- 1 %)))
         (reduce +))))

(defn weighted-cost
  [y1 y2 f]
  (let [n1 (count y1)
        n2 (count y2)
        c1 (f y1)
        c2 (f y2)]
    (float (+ (* (/ n1 (+ n1 n2)) c1)
              (* (/ n2 (+ n1 n2)) c2)))))

(defn categorical-partitions
  "Given a seq of k distinct values, returns the 2^{k-1}-1 possible binary
  partitions of the values into sets. Returns a trivial partition when k = 1."
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
  sorted seq of values. Returns the same seq when k = 1."
  [vals]
  (if (= (count vals) 1)
    vals
    (loop [partitions []
           v (sort vals)]
      (if (= (count v) 1)
        partitions
        (recur (conj partitions (/ (+ (first v) (second v)) 2))
               (rest v))))))

(defn splitters
  "Returns a seq of all possible splitters for feature i. A splitter is a
  predicate function that evaluates to true if an example belongs in the left
  subtree, or false if an example belongs in the right subtree, based on the
  splitting criterion."
  [x i]
  (let [domain (distinct (map #(nth % i) x))]
    (cond (number? (first domain)) (->> (numeric-partitions domain)
                                        (map (fn [s]
                                               (with-meta
                                                 (fn [x] (<= (nth x i) s))
                                                 {:decision (float s)}))))
          (string? (first domain)) (->> (categorical-partitions domain)
                                        (map (fn [[s1 s2]]
                                               (with-meta
                                                 (fn [x] (contains? s1 (nth x i)))
                                                 {:decision [s1 s2]}))))
          :else (throw (IllegalStateException. "Invalid feature type")))))

(defn best-splitter
  "Returns the splitter for the given data that minimizes cost function f."
  [f x y]
  (->> (for [i (range (count (first x)))]
         ;; Find best splitter for feature i
         (->> (splitters x i)
              (map (fn [splitter]
                     (let [data (map #(conj (vec %1) %2) x y)
                           [left right] (vals (group-by splitter data))
                           cost (weighted-cost (map last left) (map last right) f)]
                       ;; Add metadata to splitter
                       [(vary-meta splitter merge {:cost cost :feature i}) cost i])))
              (apply min-key second)))
       ;; Find best splitter amongst all features
       (reduce (fn [a b]
                 (let [[_ c1 i1] a [_ c2 i2] b]
                   (cond (< c1 c2) a
                         ;; To match the CART algorithm, break ties in cost by
                         ;; choosing splitter for feature with lower index
                         (= c1 c2) (if (< i1 i2) a b)
                         :else     b))))
       (first)))

(defn decision-tree-fit
  "Fits a decision tree to the given training data."
  ([model data]
   (decision-tree-fit model (map butlast data) (map last data)))
  ([model x y]
   (->> (if (apply = y)
          (bt/make-tree (first y))
          (let [{cost :cost} model
                splitter (best-splitter cost x y)
                data  (map #(conj (vec %1) %2) x y)
                split (group-by splitter data)
                left  (get split true)
                right (get split false)]
            (bt/make-tree splitter
                          (:parameters (decision-tree-fit model left))
                          (:parameters (decision-tree-fit model right)))))
        (assoc model :parameters))))

(defn decision-tree-predict
  "Predicts the values of example data using a decision tree."
  [model x]
  (let [{tree :parameters} model]
    (when (not (nil? tree))
      (letfn [(predict [t xi]
                       (let [val (bt/get-value t)]
                         (cond (bt/leaf? t) val
                               (val xi) (predict (bt/get-left t) xi)
                               :else    (predict (bt/get-right t) xi))))]
        (map #(predict tree %) x)))))

(defn print-decision-tree
  "Prints information about a given decision tree."
  [model]
  (println (dissoc model :parameters))
  (when (contains? model :parameters)
    (bt/print-tree (:parameters model))))

(defn make-decision-tree
  "Returns a decision tree model using the given cost function."
  [cost]
  {:cost cost})
