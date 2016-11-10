(ns lambda-ml.decision-tree-test
  (:require [clojure.test :refer :all]
            [lambda-ml.decision-tree :refer :all]))

(deftest test-gini-impurity
  (is (< (Math/abs (- (gini-impurity [:b :b :b :b :b :b]) 0)) 1E-6))
  (is (< (Math/abs (- (gini-impurity [:a :b :b :b :b :b]) 0.277778)) 1E-6))
  (is (< (Math/abs (- (gini-impurity [:a :a :a :b :b :b]) 0.5)) 1E-6)))

(deftest test-binary-partitions
  (let [p1 (binary-partitions [:high :normal])
        p2 (binary-partitions [:sunny :overcast :rain])
        p3 (binary-partitions [:A :B :C :D])
        partitions-equal? (fn [p1 p2]
                            (or (= p1 p2)
                                (= p1 (reverse p2))))]
    (is (= (count p1) 1))
    (is (= (count p2) 3))
    (is (= (count p3) 7))
    (is (some #(partitions-equal? % [#{:A} #{:B :C :D}]) p3))
    (is (some #(partitions-equal? % [#{:A :B} #{:C :D}]) p3))
    (is (some #(partitions-equal? % [#{:A :C} #{:B :D}]) p3))
    (is (some #(partitions-equal? % [#{:B :C} #{:A :D}]) p3))
    (is (some #(partitions-equal? % [#{:B} #{:A :C :D}]) p3))
    (is (some #(partitions-equal? % [#{:C} #{:A :B :D}]) p3))
    (is (some #(partitions-equal? % [#{:D} #{:A :B :C}]) p3))))
