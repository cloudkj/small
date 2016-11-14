(ns lambda-ml.decision-tree
  "Decision tree learning using the Classification and Regression Trees (CART)
  algorithm."
  (:require [lambda-ml.core :as c]
            [lambda-ml.data.binary-tree :as bt]))

;; Cost functions

(defn gini-impurity
  "Returns the Gini impurity of a seq of labels."
  [labels]
  (let [total (count labels)]
    (->> (vals (frequencies labels))
         (map #(/ % total))
         (map #(* % (- 1 %)))
         (reduce +))))

(defn mean-squared-error
  "Returns the mean squared error for a seq of predictions."
  [labels predictions]
  (->> (map - labels predictions)
       (map #(* % %))
       (reduce +)
       (* (/ 1 (count predictions)))))

(defn classification-weighted-cost
  [y1 y2 f g]
  (let [n1 (count y1)
        n2 (count y2)]
    ;; Classification cost doesn't take the prediction value into account
    (cond-> 0
      (> n1 0) (+ (* (/ n1 (+ n1 n2)) (f y1)))
      (> n2 0) (+ (* (/ n2 (+ n1 n2)) (f y2))))))

(defn regression-weighted-cost
  [y1 y2 f g]
  (let [n1 (count y1)
        n2 (count y2)]
    (cond-> 0
      (> n1 0) (+ (* (/ n1 (+ n1 n2))
                     (f y1 (repeat n1 (g y1)))))
      (> n2 0) (+ (* (/ n2 (+ n1 n2))
                     (f y2 (repeat n2 (g y2))))))))

;; Tree splitting

(defn categorical-partitions
  "Given a seq of k distinct values, returns the 2^{k-1}-1 possible binary
  partitions of the values into sets."
  [vals]
  (if (<= (count vals) 1)
    []
    (reduce (fn [p [s1 s2]]
              (conj p
                    [(conj s1 (first vals)) s2]
                    [(conj s2 (first vals)) s1]))
            (vector [(hash-set (first vals)) (set (rest vals))])
            (categorical-partitions (rest vals)))))

(defn numeric-partitions
  "Given a seq of k distinct numeric values, returns k-1 possible binary
  partitions of the values by taking the average of consecutive elements in the
  sorted seq of values."
  [vals]
  (loop [partitions []
         v (sort vals)]
    (if (<= (count v) 1)
      partitions
      (recur (conj partitions (/ (+ (first v) (second v)) 2))
             (rest v)))))

(defn splitters
  "Returns a seq of all possible splitters for feature i. A splitter is a
  predicate function that evaluates to true if an example belongs in the left
  subtree, or false if an example belongs in the right subtree, based on the
  splitting criterion."
  [x i]
  (let [domain (distinct (map #(nth % i) x))
        val (first domain)]
    (cond (number? val)      (->> (numeric-partitions domain)
                                  (map (fn [s]
                                         (with-meta
                                           (fn [x] (<= (nth x i) s))
                                           {:decision (float s)}))))
          (or (keyword? val)
              (string? val)) (->> (categorical-partitions domain)
                                   (map (fn [[s1 s2]]
                                          (with-meta
                                            (fn [x] (contains? s1 (nth x i)))
                                            {:decision [s1 s2]}))))
          :else (throw (IllegalArgumentException. "Invalid feature type")))))

(defn best-splitter
  "Returns the splitter for the given data that minimizes a weighted cost
  function f, or returns nil if no splitter exists."
  [f x y]
  (->> (for [i (range (count (first x)))]
         ;; Find best splitter for feature i
         (let [s (map (fn [splitter]
                        (let [data (map #(conj (vec %1) %2) x y)
                              [left right] (vals (group-by splitter data))
                              cost (f (map last left) (map last right))
                              ;; Add metadata to splitter
                              splitter (vary-meta splitter merge {:cost (float cost) :feature i})]
                          [splitter cost i]))
                      (splitters x i))]
           (if (empty? s)
             [nil Double/MAX_VALUE i]
             (apply min-key second s))))
       ;; Find best splitter amongst all features
       (reduce (fn [a b]
                 (let [[_ c1 i1] a [_ c2 i2] b]
                   (cond (< c1 c2) a
                         ;; To match the CART algorithm, break ties in cost by
                         ;; choosing splitter for feature with lower index
                         (= c1 c2) (if (< i1 i2) a b)
                         :else     b))))
       (first)))

;; API

(defn decision-tree-fit
  "Fits a decision tree to the given training data."
  ([model data]
   (decision-tree-fit model (map butlast data) (map last data)))
  ([model x y]
   (let [{cost :cost prediction :prediction weighted :weighted} model
         weighted (fn [left right] (weighted left right cost prediction))]
     ;; Stopping criteria
     (->> (if (apply = y)
            (bt/make-tree (prediction y))
            (let [splitter (best-splitter weighted x y)]
              (if (nil? splitter)
                (bt/make-tree (prediction y))
                (let [data  (map #(conj (vec %1) %2) x y)
                      split (group-by splitter data)
                      left  (get split true)
                      right (get split false)]
                  (bt/make-tree splitter
                                (:parameters (decision-tree-fit model left))
                                (:parameters (decision-tree-fit model right)))))))
          (assoc model :parameters)))))

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

(defn make-classification-tree
  "Returns a classification decision tree model using the given cost function."
  [cost]
  {:cost cost :prediction c/mode :weighted classification-weighted-cost})

(defn make-regression-tree
  "Returns a regression decision tree model using the given cost function."
  [cost]
  {:cost cost :prediction c/mean :weighted regression-weighted-cost})
