(ns lambda-ml.decision-tree-test
  (:require [clojure.test :refer :all]
            [lambda-ml.decision-tree :refer :all]))

(deftest test-gini-impurity
  (is (< (Math/abs (- (gini-impurity [:b :b :b :b :b :b]) 0)) 1E-6))
  (is (< (Math/abs (- (gini-impurity [:a :b :b :b :b :b]) 0.277778)) 1E-6))
  (is (< (Math/abs (- (gini-impurity [:a :a :a :b :b :b]) 0.5)) 1E-6)))

(deftest test-numeric-partitions
  (let [eq? (fn [a b]
              (->> (map (fn [x y] (Math/abs (- x y))) a b)
                   (every? #(< % 1E-6))))]
    (is (eq? (numeric-partitions (range 1)) [0.5]))
    (is (eq? (numeric-partitions (range 4)) [0.5 1.5 2.5]))
    (is (eq? (numeric-partitions (range 5)) [0.5 1.5 2.5 3.5]))))
