(ns test-pfilter
  (:require [clojure.test :refer :all]
            [pfilter :as core]))

(defn approx=
  "Returns true if a and b are approximately equal within epsilon."
  [a b & [eps]]
  (let [eps (or eps 1e-9)]
    (< (^[double] Math/abs (- a b)) eps)))

(deftest filter-parallel
  (testing "parallel filter works as non-parallel"
    (let [s (range 1000)
          f (filter even? s)
          pf (core/pfilter even? 10 s)]
      (is (= f pf)))))

(deftest filter-parallel-lazy
  (testing "parallel filter works eager, e.g. no lazy computations"
    (let [cnt (atom 0)
          s (range 1000)
          f (fn [x] (swap! cnt inc) (even? x))
          r (core/pfilter f 10 s)]
      (is (not (= 1000 @cnt)))
      (is (= 500 (count r)))
      (is (= 1000 @cnt)))))