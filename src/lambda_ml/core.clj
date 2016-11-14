(ns lambda-ml.core
  (:require [clojure.math.numeric-tower :refer :all]))

(def vector-with-intercept (comp vec (partial cons 1.0)))

(defn dot-product
  [a b]
  (reduce + (map * a b)))

(defn l2-norm
  [a]
  (sqrt (dot-product a a)))

(defn mean
  [coll]
  (/ (reduce + coll) (count coll)))

(defn mode
  [coll]
  (first (apply max-key second (frequencies coll))))

(defn sigmoid
  [z]
  (/ 1 (+ 1 (expt Math/E (- z)))))

(defn random-partition
  "Returns n partitions of elements randomly selected from coll."
  [n coll]
  (let [size (quot (count coll) n)
        coll (shuffle coll)]
    (partition size size [] coll)))

(defn sample
  "Returns n randomly selected elements, without replacement, from coll."
  ([coll n]
   (sample coll n (list)))
  ([coll n s]
   (if-not (vector? coll)
     (sample (vec coll) n s)
     (let [index (rand-int (count coll))]
       (if (or (<= n 0) (empty? coll))
         s
         (sample (subvec (assoc coll index (first coll)) 1)
                 (dec n)
                 (conj s (nth coll index))))))))
