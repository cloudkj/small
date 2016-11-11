(ns lambda-ml.decision-tree-test
  (:require [clojure.test :refer :all]
            [lambda-ml.decision-tree :refer :all]))

(deftest test-gini-impurity
  (is (< (Math/abs (- (gini-impurity [:b :b :b :b :b :b]) 0)) 1E-6))
  (is (< (Math/abs (- (gini-impurity [:a :b :b :b :b :b]) 0.277778)) 1E-6))
  (is (< (Math/abs (- (gini-impurity [:a :a :a :b :b :b]) 0.5)) 1E-6)))

(deftest test-weighted-cost
  (is (< (Math/abs (- (weighted-cost [:a :a :a :b :b :b] [:a :b :b :b :b :b] gini-impurity) 0.388889)) 1E-6)))

(deftest test-categorical-partitions
  (let [p0 (categorical-partitions [:foo])
        p1 (categorical-partitions [:high :normal])
        p2 (categorical-partitions [:sunny :overcast :rain])
        p3 (categorical-partitions [:A :B :C :D])
        partitions-equal? (fn [p1 p2]
                            (or (= p1 p2)
                                (= p1 (reverse p2))))]
    (is (= (count p0) 1))
    (is (= (count p1) 1))
    (is (= (count p2) 3))
    (is (= (count p3) 7))
    (is (some #(partitions-equal? % [#{:foo} #{}]) p0))
    (is (some #(partitions-equal? % [#{:A} #{:B :C :D}]) p3))
    (is (some #(partitions-equal? % [#{:A :B} #{:C :D}]) p3))
    (is (some #(partitions-equal? % [#{:A :C} #{:B :D}]) p3))
    (is (some #(partitions-equal? % [#{:B :C} #{:A :D}]) p3))
    (is (some #(partitions-equal? % [#{:B} #{:A :C :D}]) p3))
    (is (some #(partitions-equal? % [#{:C} #{:A :B :D}]) p3))
    (is (some #(partitions-equal? % [#{:D} #{:A :B :C}]) p3))))

(deftest test-numeric-partitions
  (let [eq? (fn [a b]
              (->> (map (fn [x y] (Math/abs (- x y))) a b)
                   (every? #(< % 1E-6))))]
    (is (= (numeric-partitions [42]) [42]))
    (is (eq? (numeric-partitions (range 1)) [0]))
    (is (eq? (numeric-partitions (range 4)) [0.5 1.5 2.5]))
    (is (eq? (numeric-partitions (range 5)) [0.5 1.5 2.5 3.5]))
    (is (eq? (numeric-partitions [1 0]) [0.5]))
    (is (eq? (numeric-partitions [2 1 0 3]) [0.5 1.5 2.5]))
    (is (eq? (numeric-partitions [3 4 1 2 0]) [0.5 1.5 2.5 3.5]))))

(deftest test-categorical-splitters
  (let [data [["Sunny" "Hot" "High" "Weak" "No"]
              ["Sunny" "Hot" "High" "Strong" "No"]
              ["Overcast" "Hot" "High" "Weak" "Yes"]
              ["Rain" "Mild" "High" "Weak" "Yes"]
              ["Rain" "Cool" "Normal" "Weak" "Yes"]
              ["Rain" "Cool" "Normal" "Strong" "No"]
              ["Overcast" "Cool" "Normal" "Strong" "Yes"]
              ["Sunny" "Mild" "High" "Weak" "No"]
              ["Sunny" "Cool" "Normal" "Weak" "Yes"]
              ["Rain" "Mild" "Normal" "Weak" "Yes"]
              ["Sunny" "Mild" "Normal" "Strong" "Yes"]
              ["Overcast" "Mild" "High" "Strong" "Yes"]
              ["Overcast" "Hot" "Normal" "Weak" "Yes"]
              ["Rain" "Mild" "High" "Strong" "No"]]]
    (is (= (count (splitters data 0)) 3))
    (is (= (count (splitters data 1)) 3))
    (is (= (count (splitters data 2)) 1))
    (is (= (count (splitters data 3)) 1))))

(deftest test-numeric-splitters
  (let [data [[64 177]
              [65 255]
              [85 125]
              [80 60]
              [72 56]
              [75 120]
              [75 100]
              [68 220]
              [71 90]
              [83 95]
              [69 52]
              [70 70]
              [72 85]
              [81 75]]]
    (is (= (count (splitters data 0)) 11))
    (is (= (count (splitters data 1)) 13))))

(deftest test-best-splitter-categorical
  (let [data [["Sunny" "Hot" "High" "Weak" "No"]
              ["Sunny" "Hot" "High" "Strong" "No"]
              ["Overcast" "Hot" "High" "Weak" "Yes"]
              ["Rain" "Mild" "High" "Weak" "Yes"]
              ["Rain" "Cool" "Normal" "Weak" "Yes"]
              ["Rain" "Cool" "Normal" "Strong" "No"]
              ["Overcast" "Cool" "Normal" "Strong" "Yes"]
              ["Sunny" "Mild" "High" "Weak" "No"]
              ["Sunny" "Cool" "Normal" "Weak" "Yes"]
              ["Rain" "Mild" "Normal" "Weak" "Yes"]
              ["Sunny" "Mild" "Normal" "Strong" "Yes"]
              ["Overcast" "Mild" "High" "Strong" "Yes"]
              ["Overcast" "Hot" "Normal" "Weak" "Yes"]
              ["Rain" "Mild" "High" "Strong" "No"]]
        splitter (best-splitter gini-impurity (map butlast data) (map last data))
        [left right] (vals (group-by splitter data))]
    (is (or (and (= (count left) 10) (= (count right) 4))
            (and (= (count left) 4) (= (count right) 10))))))
