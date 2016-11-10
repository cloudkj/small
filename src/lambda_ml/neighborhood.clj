(ns lambda-ml.neighborhood
  (:require [lambda-ml.data.binary-tree :as bt]
            [lambda-ml.data.kd-tree :as kd]
            [lambda-ml.data.priority-queue :as pq]))

;; K-nearest neighbors

(defn make-knn
  "Given a distance function f and a coll of items, each of which have an
  associated dimensional point, returns a function that, given k and a query
  item, returns a priority queue of the k nearest neighboring items. Optionally,
  a function g can be supplied and used to return the dimensional point for an
  item. Otherwise, the item itself is assumed to be the point. Assumes that all
  points are represented as sequences of the same dimension."
  ([f items]
   (make-knn f identity items))
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
               ;; Determine near and far branches
               [near far] (if (<= (nth (g query) dim) (nth (g node) dim)) [left right] [right left])
               cand (->>
                     ;; Try to add current node to candidates
                     (pq/insert cand node (f (g query) (g node)) k)
                     ;; Explore near branch
                     (knn k query near (inc depth)))]
           ;; Optionally, explore far branch
           (if (or (< (count cand) k)
                   (< (f (g query) (g node) dim) (pq/item-priority (pq/get-tail cand))))
             (knn k query far (inc depth) cand)
             cand))))))))

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
