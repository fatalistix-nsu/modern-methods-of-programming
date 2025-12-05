(ns test-dnf
  (:require [clojure.test :refer :all])
  (:require [ast])
  (:require [dnf])
  (:require [test-util :refer :all]))

(deftest push-negations-not
  (testing "negations pushing on not node"
    (let [i (ast/->Not (ast/->And (ast/->True) (ast/->Var :name)))
          o (ast/->Or (ast/->Not (ast/->True)) (ast/->Not (ast/->Var :name)))
          r (dnf/push-negations i)]
      (is (ast/eq o r)))))

(deftest remove-double-negations-not
  (testing "removing double negations on not node"
    (let [i (ast/->Not (ast/->Not (ast/->True)))
          o (ast/->True)
          r (dnf/remove-double-negations i)]
      (is (ast/eq o r)))))

