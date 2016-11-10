(ns lambda-ml.data.kd-tree-test
  (:require [clojure.test :refer :all]
            [lambda-ml.data.binary-tree :as bt]
            [lambda-ml.data.kd-tree :refer :all]))

(deftest test-kd-tree
  (let [tree (make-tree 2 [[2 3] [5 4] [9 6] [4 7] [8 1] [7 2]])]
    (is (= (bt/get-value tree) [7 2]))
    (is (= (bt/get-path tree [:left])  (bt/get-left tree)))
    (is (= (bt/get-path tree [:right]) (bt/get-right tree)))
    (is (= (bt/get-path tree [:left :left])  (-> tree bt/get-left bt/get-left)))
    (is (= (bt/get-path tree [:left :right]) (-> tree bt/get-left bt/get-right)))
    (is (= (bt/get-path tree [:right :left]) (-> tree bt/get-right bt/get-left)))
    (is (= (bt/get-value (bt/get-path tree [:left]))         [5 4]))
    (is (= (bt/get-value (bt/get-path tree [:right]))        [9 6]))
    (is (= (bt/get-value (bt/get-path tree [:left  :left]))  [2 3]))
    (is (= (bt/get-value (bt/get-path tree [:left  :right])) [4 7]))
    (is (= (bt/get-value (bt/get-path tree [:right :left]))  [8 1]))
    (is (nil? (bt/get-path tree [:left :left :left])))
    (is (nil? (bt/get-path tree [:left :left :right])))
    (is (nil? (bt/get-path tree [:left :right :left])))
    (is (nil? (bt/get-path tree [:left :right :right])))
    (is (nil? (bt/get-path tree [:right :left :left])))
    (is (nil? (bt/get-path tree [:right :left :right])))
    (is (nil? (bt/get-path tree [:right :right])))))
