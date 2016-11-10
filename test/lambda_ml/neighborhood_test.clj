(ns lambda-ml.neighborhood-test
  (:require [clojure.test :refer :all]
            [clojure.set :refer :all]
            [lambda-ml.neighborhood :refer :all]
            [lambda-ml.distance :as d]
            [lambda-ml.data.priority-queue :as pq]))

(deftest test-knn
  (let [knn (make-knn d/euclidean [[2 3] [5 4] [9 6] [4 7] [8 1] [7 2]])]
    (is (= [7 2] (pq/item-value (second (knn 2 [8 1])))))
    (is (= [5 4] (pq/item-value (second (knn 2 [2 3])))))
    (is (= [8 1] (pq/item-value (second (knn 2 [7 2])))))
    (is (= [5 4] (pq/item-value (second (knn 2 [4 7])))))
    (is (= 3 (count (knn 3 [2 3]))))
    (is (= 6 (count (knn 6 [2 3]))))
    (is (= 6 (count (knn 9 [2 3]))))))

(deftest test-knn2
  (let [knn (make-knn d/euclidean [[1 11] [2 5] [4 8] [6 4] [5 0] [7 9] [8 2]])]
    (is (= [4 8] (pq/item-value (first (knn 5 [3 9])))))))

(deftest test-knn3
  (let [points {[0.0   0.0]    1
                [10.1  -10.1]  2
                [-12.2 12.2]   3
                [38.3  38.3]   4
                [79.99 179.99] 5}
        knn (make-knn d/euclidean (keys points))]
    (is (= (list 1 2 3 4)
           (map (comp points second) (knn 4 ((map-invert points) 1)))))
    (is (= (list 2 1 3 4)
           (map (comp points second) (knn 4 ((map-invert points) 2)))))
    (is (= (list 3 1 2 4)
           (map (comp points second) (knn 4 ((map-invert points) 3)))))
    (is (= (list 4 1 2 3)
           (map (comp points second) (knn 4 ((map-invert points) 4)))))
    (is (= (list 5 4 3 1)
           (map (comp points second) (knn 4 ((map-invert points) 5)))))))

(deftest test-knn4
  (let [points {[0.436697697345292 0.492281587956396] 1
                [0.318000697283004 0.302602867518914] 2
                [0.268674100320323 0.684132163547525] 3
                [0.347190228888873 0.959920716313895] 4
                [0.539212291014011 0.187100169547265] 5
                [0.964631186098456 0.129079314315528] 6
                [0.171792010609788 0.795749621321345] 7
                [0.910157297130659 0.437962722965556] 8
                [0.847975159955406 0.169625495659256] 9
                [0.793504465072615 0.121750314432942] 10}
        knn (make-knn d/euclidean (keys points))]
    (is (= (list 1 2 3 5)
           (map (comp points second) (knn 4 ((map-invert points) 1)))))
    (is (= (list 2 1 5 3)
           (map (comp points second) (knn 4 ((map-invert points) 2)))))
    (is (= (list 3 7 1 4)
           (map (comp points second) (knn 4 ((map-invert points) 3)))))
    (is (= (list 4 7 3 1)
           (map (comp points second) (knn 4 ((map-invert points) 4)))))
    (is (= (list 5 2 10 9)
           (map (comp points second) (knn 4 ((map-invert points) 5)))))
    (is (= (list 6 9 10 8)
           (map (comp points second) (knn 4 ((map-invert points) 6)))))
    (is (= (list 7 3 4 1)
           (map (comp points second) (knn 4 ((map-invert points) 7)))))
    (is (= (list 8 9 6 10)
           (map (comp points second) (knn 4 ((map-invert points) 8)))))
    (is (= (list 9 10 6 8)
           (map (comp points second) (knn 4 ((map-invert points) 9)))))
    (is (= (list 10 9 6 5)
           (map (comp points second) (knn 4 ((map-invert points) 10)))))))

(deftest test-knn5
  (let [points {[36.971838 -122.019653] :SantaCruz,
                [37.864012 -122.277832] :Berkeley,
                [37.330857 -121.887817] :SanJose,
                [37.444335 -122.156982] :PaloAlto,
                [37.387617 -122.060852] :MountainView,
                [37.759859 -122.437134] :SanFrancisco}
        knn (make-knn d/euclidean (keys points))]
    (is (= :SanJose
           (-> (knn 2 ((map-invert points) :SantaCruz)) second pq/item-value points)))
    (is (= :SanFrancisco
           (-> (knn 2 ((map-invert points) :Berkeley)) second pq/item-value points)))
    (is (= :MountainView
           (-> (knn 2 ((map-invert points) :PaloAlto)) second pq/item-value points)))))

(deftest test-knn-metadata
  (let [points [[:a 2 3]
                [:b 5 4]
                [:c 9 6]
                [:d 4 7]
                [:e 8 1]
                [:f 7 2]]
        knn (make-knn d/euclidean rest points)]
    (is (= :f (first (pq/item-value (second (knn 2 [:e 8 1]))))))
    (is (= :b (first (pq/item-value (second (knn 2 [:a 2 3]))))))
    (is (= :e (first (pq/item-value (second (knn 2 [:f 7 2]))))))
    (is (= :b (first (pq/item-value (second (knn 2 [:d 4 7]))))))))

(deftest test-search
  (let [points {:SanFrancisco [37.759859 -122.437134]
                :Berkeley     [37.864012 -122.277832]
                :PaloAlto     [37.444335 -122.156982]
                :MountainView [37.387617 -122.060852]
                :SanJose      [37.330857 -121.887817]
                :SantaCruz    [36.971838 -122.019653]}
        locations (map-invert points)
        search (make-search d/haversine (vals points))]
    (is (= (set (map locations (search 1 (points :MountainView))))
           #{:MountainView}))
    (is (= (set (map locations (search 2 (points :MountainView))))
           #{:MountainView}))
    (is (= (set (map locations (search 4 (points :MountainView))))
           #{:MountainView}))
    (is (= (set (map locations (search 8 (points :MountainView))))
           #{:MountainView :PaloAlto}))
    (is (= (set (map locations (search 16 (points :MountainView))))
           #{:MountainView :PaloAlto :SanJose}))
    (is (= (set (map locations (search 30 (points :MountainView))))
           #{:MountainView :PaloAlto :SanJose :SantaCruz}))
    (is (= (set (map locations (search 35 (points :MountainView))))
           #{:MountainView :PaloAlto :SanJose :SantaCruz :SanFrancisco}))
    (is (= (set (map locations (search 50 (points :MountainView))))
           #{:MountainView :PaloAlto :SanJose :SantaCruz :SanFrancisco :Berkeley}))))
