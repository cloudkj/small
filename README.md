# lambda-ml

A small machine learning library aimed at providing simple, concise
implementations of machine learning techniques and utilities. It is written in
Lisp (using the implementation du jour, Clojure) to maximize expressiveness and
enjoyment.

## Documentation

* [API Docs](https://cloudkj.github.io/lambda-ml/)

## Installation

Add the following dependency to your project:

[![Clojars Project](https://img.shields.io/clojars/v/lambda-ml.svg)](https://clojars.org/lambda-ml)

## Examples

### Artificial neural network

```clojure
(ns example
  (:require [lambda-ml.neural-network :refer :all]))

(def data [[0 0 0] [0 1 1] [1 0 1] [1 1 0]])
(def hidden-layers [3])
(def alpha 0.5)
(def model (make-neural-network hidden-layers alpha))

(def fit
  (-> #(neural-network-fit % (map butlast data) (map (comp vector last) data))
      (iterate model)
      (nth 5000)))

(neural-network-predict fit (map butlast data))
;;=> ((0.018947665113895507) (0.9768421723034613) (0.9770467568718264) (0.03108874342427228))
```

### DBSCAN

```clojure
(ns example
 (:require [lambda-ml.clustering.dbscan :refer :all]
           [lambda-ml.distance :refer :all]))

(def epsilon 4.0)
(def min-pts 2)
(def data [[2 10] [2 5] [8 4] [5 8] [7 5] [6 4] [1 2] [4 9]])

(dbscan euclidean epsilon min-pts data)
;;=> {[4 9] 2, [5 8] 2, [7 5] 1, [6 4] 1, [8 4] 1}
```

### Decision Tree

```clojure
(ns example
  (:require [lambda-ml.decision-tree :refer :all]
            [lambda-ml.metrics :refer :all]))

(def data [[0 0 0] [0 1 1] [1 0 1] [1 1 0]])
(def model (make-classification-tree gini-impurity))
(def fit (decision-tree-fit model data))

(decision-tree-predict fit (map butlast data))
;;=> (0 1 1 0)
```

### K-means

```clojure
(ns example
 (:require [lambda-ml.clustering.k-means :refer :all]
           [lambda-ml.distance :refer :all]))

(def k 2)
(def iters 100)
(def data [[1 1] [1.5 2] [3 4] [5 7] [3.5 5] [4.5 5] [3.5 4.5]])

(nth (k-means k euclidean data) iters)
;;=> {0 ([3.5 4.5] [4.5 5] [3.5 5] [5 7] [3 4]), 1 ([1.5 2] [1 1])}
```

### K-nearest neighbors

```clojure
(ns example
 (:require [lambda-ml.neighborhood :refer :all]
           [lambda-ml.distance :refer :all]))

(def data [[2 3] [5 4] [9 6] [4 7] [8 1] [7 2]])
(def knn (make-knn euclidean data))

(knn 3 [8 1])
;;=> [[0 [8 1]] [2 [7 2]] [18 [5 4]]]
```

### Linear regression

```clojure
(ns example
 (:require [lambda-ml.regression :refer :all]))

(def data [[-2 -1] [1 1] [3 2]])
(def model (make-linear-regression 0.01 0 5000))
(def fit (regression-fit model data))

(fit :parameters)
;;=> (0.26315789473683826 0.6052631578947385)
```

### Logistic regression

```clojure
(ns example
 (:require [lambda-ml.regression :refer :all]))

(def data [[0.50 0] [0.75 0] [1.00 0] [1.25 0] [1.50 0] [1.75 0] [1.75 1] [2.00 0] [2.25 1] [2.50 0] [2.75 1] [3.00 0] [3.25 1] [3.50 0] [4.00 1] [4.25 1] [4.50 1] [4.75 1] [5.00 1] [5.50 1]])
(def model (make-logistic-regression 0.1 0 10000))
(def fit (regression-fit model data))

(fit :parameters)
;;=> (-4.077712444728931 1.5046450944762417)
```

### Naive Bayes

```clojure
(ns example
 (:require [lambda-ml.naive-bayes :refer :all]))

(def x [[6.0 180 12] [5.92 190 11] [5.58 170 12] [5.92 165 10] [5.0 100 6] [5.5 150 8] [5.42 130 7] [5.75 150 9]])
(def y [:male :male :male :male :female :female :female :female])
(def model (naive-bayes-fit x y))

(naive-bayes-predict model [[6.0 130 8]])
;;=> (:female)
```

## Contents

### Algorithms

* [Artificial neural network](https://cloudkj.github.io/lambda-ml/lambda-ml.neural-network.html)
* [DBSCAN](https://cloudkj.github.io/lambda-ml/lambda-ml.clustering.dbscan.html)
* [Decision tree](https://cloudkj.github.io/lambda-ml/lambda-ml.decision-tree.html)
* [K-means](https://cloudkj.github.io/lambda-ml/lambda-ml.clustering.k-means.html)
* [K-nearest neighbors](https://cloudkj.github.io/lambda-ml/lambda-ml.neighborhood.html)
* [Linear regression](https://cloudkj.github.io/lambda-ml/lambda-ml.regression.html)
* [Logistic regression](https://cloudkj.github.io/lambda-ml/lambda-ml.regression.html)
* [Naive Bayes](https://cloudkj.github.io/lambda-ml/lambda-ml.naive-bayes.html)

### Data Structures

* [Binary tree](https://cloudkj.github.io/lambda-ml/lambda-ml.data.binary-tree.html)
* [K-d tree](https://cloudkj.github.io/lambda-ml/lambda-ml.data.kd-tree.html)
* [Priority queue](https://cloudkj.github.io/lambda-ml/lambda-ml.data.priority-queue.html)

## License

Copyright © 2015-2016
