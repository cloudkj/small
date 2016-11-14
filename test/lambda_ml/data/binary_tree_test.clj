(ns lambda-ml.data.binary-tree-test
  (:require [clojure.test :refer :all]
            [lambda-ml.data.binary-tree :refer :all]))

(deftest test-binary-tree-leaf
  (let [tree (make-tree 42)]
    (is (= (get-value tree) 42))
    (is (nil? (get-left tree)))
    (is (nil? (get-right tree)))
    (is (leaf? tree))))

(deftest test-binary-tree
  (let [tree (make-tree 2
                        (make-tree 7
                                   (make-tree 2)
                                   (make-tree 6 (make-tree 5) (make-tree 11)))
                        (make-tree 5
                                   nil
                                   (make-tree 9 (make-tree 4) nil)))]
    (is (= (get-value tree) 2))
    (is (= (get-path tree [:left])  (get-left tree)))
    (is (= (get-path tree [:right]) (get-right tree)))
    (is (= (get-value (get-path tree [:left :right :left])) 5))
    (is (= (get-value (get-path tree [:right :right :left])) 4))))

(deftest test-adjacency-matrix
  (let [tree (make-tree :a
                        (make-tree :b
                                   (make-tree :c)
                                   (make-tree :d (make-tree :e) (make-tree :f)))
                        (make-tree :g
                                   nil
                                   (make-tree :h (make-tree :i) nil)))
        matrix (adjacency-matrix tree)]
    (is (= (count matrix) 9))
    (is (empty? (:edges (first (filter #(= :c (:value %)) (vals matrix))))))
    (is (empty? (:edges (first (filter #(= :e (:value %)) (vals matrix))))))
    (is (empty? (:edges (first (filter #(= :f (:value %)) (vals matrix))))))
    (is (empty? (:edges (first (filter #(= :i (:value %)) (vals matrix))))))
    (is (= (count (:edges (first (filter #(= :a (:value %)) (vals matrix))))) 2))
    (is (= (count (:edges (first (filter #(= :b (:value %)) (vals matrix))))) 2))
    (is (= (count (:edges (first (filter #(= :d (:value %)) (vals matrix))))) 2))
    (is (= (count (:edges (first (filter #(= :g (:value %)) (vals matrix))))) 1))
    (is (= (count (:edges (first (filter #(= :h (:value %)) (vals matrix))))) 1))))
