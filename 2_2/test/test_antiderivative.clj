(ns test-antiderivative
  (:require [clojure.test :refer :all]
            [antiderivative :as core]))

(defn approx=
  "Returns true if a and b are approximately equal within epsilon."
  [a b & [eps]]
  (let [eps (or eps 1e-9)]
    (< (^[double] Math/abs (- a b)) eps)))

(deftest antiderivative-constant
  (testing "f(x) = C ==> F(x) = x * C"
    (let [c [0 1 1.5]]
      (doseq [c c]
        (let [h [0.5 1]]
          (doseq [h h]
            (let [x [0 1.5 1 10]]
              (doseq [x x]
                (let [f (core/antiderivative (fn [_] c) h)]
                  (is (approx= (f x) (* c x))))))))))))

(deftest antiderivative-linear
  (testing "f(x) = x ==> F(x) = x^2/2"
    (let [h [0.5 1]]
      (doseq [h h]
        (let [x [0 1.5 1 10]]
          (doseq [x x]
            (let [f (core/antiderivative (fn [x] x) h)]
              (is (approx= (f x) (/ (* x x) 2))))))))))

(deftest memoization-match
  (testing "when f(x) matches memoization atom is not changed"
    (let [cnt (atom 0)
          f (core/antiderivative (fn [_] (swap! cnt inc) 1) 1)]
      (f 2)
      (is (= 3 @cnt) "function calculated for values 0, 1, 2 only once")
      (f 2)
      (is (= 3 @cnt) "memoization matched")
      (f 1)
      (is (= 3 @cnt) "memoization matched"))))
