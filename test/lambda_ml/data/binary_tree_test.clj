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
