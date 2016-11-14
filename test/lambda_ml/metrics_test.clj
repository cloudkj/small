(ns lambda-ml.metrics-test
  (:require [clojure.test :refer :all]
            [lambda-ml.metrics :refer :all]))

(deftest test-auc
  (is (= 0.75 (auc [[0 0.5] [0.5 0.5] [0.5 1] [1 1]])))
  (is (= 147.66 (auc [[0 100] [1 50] [2 25] [3 12.5] [4 6.25] [5 3.13] [6 1.56]]))))
