(ns test-ast
  (:require [clojure.test :refer :all])
  (:require [ast])
  (:require [test-util :refer :all]))

(deftest verify-substitution-ok
  (testing "corrective of given substitution"
    (let [k :name
          v true]
      (try
        (ast/verify-substitution k v)
        (catch Exception e
          (fail e))))))

(deftest verify-substitution-wrong-key
  (testing "exception throwing on non-keyword k"
    (let [k "name"
          v true]
      (try
        (ast/verify-substitution k v)
        (fail)
        (catch Exception e
          (is "key is not a keyword" (ex-message e))
          (is "name" (:k (ex-cause e))))))))

(deftest verify-substitution-wrong-value
  (testing "exception throwing on non-boolean v"
    (let [k :name
          v 1]
      (try
        (ast/verify-substitution k v)
        (fail)
        (catch Exception e
          (is "value is not a boolean" (ex-message e))
          (is 1 (:v (ex-cause e))))))))

(deftest eq-var
  (testing "equality for vars"
    (let [a (ast/->Var :name)
          b (ast/->Var :name)]
      (is (ast/eq a b)))))

(deftest not-eq-var
  (testing "non-equality for vars which names differ"
    (let [a (ast/->Var :a)
          b (ast/->Var :b)]
      (is (not (ast/eq a b))))))

(deftest eq-and
  (testing "equality for and"
    (let [a (ast/->And (ast/->True) (ast/->Var :name))
          b (ast/->And (ast/->True) (ast/->Var :name))]
      (is (ast/eq a b)))

    (let [a (ast/->And (ast/->False) (ast/->Var :name))
          b (ast/->And (ast/->Var :name) (ast/->False))]
      (is (ast/eq a b)))))

(deftest not-eq-and
  (testing "non-equality for and with different args"
    (let [a (ast/->And (ast/->True) (ast/->True))
          b (ast/->And (ast/->True) (ast/->False))]
      (is (not (ast/eq a b))))))

(deftest eq-or
  (testing "equality for or"
    (let [a (ast/->Or (ast/->True) (ast/->Var :name))
          b (ast/->Or (ast/->True) (ast/->Var :name))]
      (is (ast/eq a b)))

    (let [a (ast/->Or (ast/->False) (ast/->Var :name))
          b (ast/->Or (ast/->Var :name) (ast/->False))]
      (is (ast/eq a b)))))

(deftest eq-not
  (testing "equality for not"
    (let [a (ast/->Not (ast/->True))
          b (ast/->Not (ast/->True))]
      (is (ast/eq a b)))))