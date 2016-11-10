(ns lambda-ml.data.kd-tree
  (:require [lambda-ml.data.binary-tree :as bt]))

;; K-d tree

(defn make-tree
  "Returns a k-d tree, with dims as the number of dimensions, for the given
  nodes. Optionally, a function f can be supplied and used to return the
  k-dimensional point for a given node. Otherwise, the node itself is assumed to
  be the k-dimensional point."
  ([dims nodes]
   (make-tree dims nodes identity))
  ([dims nodes f]
   (make-tree dims nodes f 0))
  ([dims nodes f depth]
   (if (empty? nodes)
     nil
     (let [dim (fn [node] (nth (f node) (mod depth dims)))
           sorted (sort-by dim nodes)
           median (quot (count sorted) 2)]
       (bt/make-tree (nth sorted median)
                     (make-tree dims (take median sorted) f (inc depth))
                     (make-tree dims (drop (+ median 1) sorted) f (inc depth)))))))
