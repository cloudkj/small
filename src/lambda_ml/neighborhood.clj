(ns lambda-ml.neighborhood
  (:require [lambda-ml.core :refer :all]
            [lambda-ml.data.binary-tree :as bt]
            [lambda-ml.data.kd-tree :as kd]
            [lambda-ml.data.priority-queue :as pq]))

;; K-nearest neighbors

(defn make-nearest-neighbor-search
  "Given a distance function f and a coll of items, each of which have an
  associated dimensional point, returns a function that, given k and a query
  item, returns a priority queue of the k nearest neighboring items. Optionally,
  a function g can be supplied and used to return the dimensional point for an
  item. Otherwise, the item itself is assumed to be the point. Assumes that all
  points are represented as sequences of the same dimension."
  ([f items]
   (make-nearest-neighbor-search f identity items))
  ([f g items]
  (let [dims (count (g (first items)))
        t (kd/make-tree dims items g)]
    (fn knn
      ([k query]
       (knn k query t 0 (pq/make-queue)))
      ([k query tree depth cand]
       (if (nil? tree)
         cand
         (let [[node left right] ((juxt bt/get-value bt/get-left bt/get-right) tree)
               dim (mod depth dims)
               node-point (g node)
               query-point (g query)
               ;; Determine near and far branches
               [near far] (if (<= (nth query-point dim) (nth node-point dim)) [left right] [right left])
               cand (->>
                     ;; Try to add current node to candidates
                     (pq/insert cand node (f query-point node-point) k)
                     ;; Explore near branch
                     (knn k query near (inc depth)))]
           ;; Optionally, explore far branch
           (if (or (< (count cand) k)
                   (< (f query-point node-point dim)
                      (pq/item-priority (pq/get-tail cand))))
             (knn k query far (inc depth) cand)
             cand))))))))

(defn nearest-neighbors-fit
  "Fits a k-nearest neighbors model to the given training data."
  ([model data]
   (assoc model :lookup (make-nearest-neighbor-search (:dist model) butlast data)))
  ([model x y]
   (nearest-neighbors-fit model (map concat x (map list y)))))

(defn nearest-neighbors-predict
  "Predicts the values of example data using a k-nearest neighbors model."
  [model x]
  (let [{k :k lookup :lookup agg :aggregation} model]
    (when (not (nil? lookup))
      ;; Append dummy coordinate value to account for assumption of target
      ;; values in last position in training data examples
      (->> (map #(conj (vec %) nil) x)
           (map #(lookup k %))
           (map #(map (comp last pq/item-value) %))
           (map agg)))))

(defn make-nearest-neighbors-classifier
  "Returns a k-nearest neighbor classification model using the given distance
  function."
  [k dist]
  {:k k
   :dist dist
   :aggregation mode})

(defn make-nearest-neighbors-regressor
  "Returns a k-nearest neighbor regression model using the given distance
  function."
  [k dist]
  {:k k
   :dist dist
   :aggregation mean})

;; Proximity search

(defn make-search
  "Given a distance function f and a coll of points, returns a function that,
  given a distance and a query point, returns a sequence of all points that are
  within the given distance of the query point."
  [f points]
  (let [dims (count (first points))
        t (kd/make-tree dims points)]
    (fn search
      ([dist query]
       (search dist query t 0 (list)))
      ([dist query tree depth cand]
       (if (nil? tree)
         cand
         (let [[node left right] ((juxt bt/get-value bt/get-left bt/get-right) tree)
               dim (mod depth dims)
               [near far] (if (<= (nth query dim) (nth node dim)) [left right] [right left])]
           (cond->> cand
             ;; Add current node if it's within proximity
             (<= (f query node) dist)
             (cons node)
             ;; Explore near branch
             true
             (search dist query near (inc depth))
             ;; Optionally, explore far branch
             (< (f query node dim) dist)
             (search dist query far (inc depth)))))))))
