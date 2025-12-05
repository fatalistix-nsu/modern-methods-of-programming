(ns test-ext
  (:require [clojure.test :refer :all])
  (:require [ast])
  (:require [dnf])
  (:require [ext])
  (:require [test-util :refer :all]))

(deftest deep-implication
  (testing "(A and (B or C)) -> ((D implication E) and (F or not G)) =
            (not A or (not B and not C)) or ((((not D) and F) or ((not D) and (not G))) or ((E and F) or (E and (not G))))"
    (let [i (ext/->Implies
              (ast/->And
                (ast/make-var :a)
                (ast/->Or
                  (ast/make-var :b)
                  (ast/make-var :c)))
              (ast/->And
                (ext/->Implies
                  (ast/make-var :d)
                  (ast/make-var :e))
                (ast/->Or
                  (ast/make-var :f)
                  (ast/->Not (ast/make-var :g)))))

          o (ast/->Or
              (ast/->Or
                (ast/->Not (ast/make-var :a))
                (ast/->And
                  (ast/->Not (ast/make-var :b))
                  (ast/->Not (ast/make-var :c))))
              (ast/->Or
                (ast/->Or
                  (ast/->And
                    (ast/->Not (ast/make-var :d))
                    (ast/make-var :f))
                  (ast/->And
                    (ast/->Not (ast/make-var :d))
                    (ast/->Not (ast/make-var :g))))
                (ast/->Or
                  (ast/->And
                    (ast/make-var :e)
                    (ast/make-var :f))
                  (ast/->And
                    (ast/make-var :e)
                    (ast/->Not (ast/make-var :g))))))

          r (dnf/convert i)]
      (is (ast/eq o r)))))

(deftest deep-implication-substitute-var
  (testing "var substitution in deep implication test"
    (let [i (ext/->Implies
              (ast/->And
                (ast/make-var :a)
                (ast/->Or
                  (ast/make-var :b)
                  (ast/make-var :c)))
              (ast/->And
                (ext/->Implies
                  (ast/make-var :d)
                  (ast/make-var :e))
                (ast/->Or
                  (ast/make-var :f)
                  (ast/->Not (ast/make-var :g)))))

          o (ast/->Or
              (ast/->Or
                (ast/->Not (ast/make-var :a))
                (ast/->And
                  (ast/->Not (ast/make-var :b))
                  (ast/->Not (ast/make-var :c))))
              (ast/->Or
                (ast/->Or
                  (ast/->And
                    (ast/->Not (ast/->True))
                    (ast/make-var :f))
                  (ast/->And
                    (ast/->Not (ast/->True))
                    (ast/->Not (ast/make-var :g))))
                (ast/->Or
                  (ast/->And
                    (ast/make-var :e)
                    (ast/make-var :f))
                  (ast/->And
                    (ast/make-var :e)
                    (ast/->Not (ast/make-var :g))))))

          r (-> i
                (dnf/convert)
                (ast/substitute :d true))]
      (is (ast/eq o r)))))