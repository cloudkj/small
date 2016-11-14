(ns lambda-ml.metrics
  "Functions that compute measures of cost or gain.")

(defn auc
  "Returns the area under the curve of a given collection of points, using the
  trapezoidal rule."
  [points]
  (loop [area 0
         [x0 y0] (first points)
         points (rest points)]
    (if (empty? points)
      area
      (let [[x1 y1] (first points)
            dx (- x1 x0)]
        (recur (+ area (* dx (/ (+ y1 y0) 2)))
               [x1 y1]
               (rest points))))))

(defn roc-curve
  "Returns a sequence of [false positive rate, true positive rate] tuples that
  represent the ROC curve of a classifier."
  [labels predictions]
  (let [p (reduce + (filter (fn [x] (= x 1)) labels))
        n (- (count labels) p)
        ranked (->> (map vector labels predictions)
                    (sort-by second)
                    (map first)
                    (reverse))]
    (loop [ys ranked
           fp 0
           tp 0
           points []]
      (if (empty? ys)
        points
        (let [fp (if (= (first ys) 0) (inc fp) fp)
              tp (if (= (first ys) 1) (inc tp) tp)
              fpr (/ fp n)
              tpr (/ tp p)]
          (recur (rest ys) fp tp (conj points [fpr tpr])))))))
