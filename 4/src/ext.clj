(ns ext
  (:import (ast INode)
           (dnf IDnfEliminateOtherOps)))

(defrecord Implies [a b]
  INode
  (substitute [this k v]
    (ast/verify-substitution k v)
    (->Implies
      (ast/substitute (:a this) k v)
      (ast/substitute (:b this) k v)))

  (eq [_ other]
    (and
      (instance? Implies other)
      (ast/eq a (:a other))
      (ast/eq b (:b other))))

  IDnfEliminateOtherOps
  (eliminate-other-ops [this]
    (ast/->Or
      (ast/->Not (dnf/eliminate-other-ops (:a this)))
      (dnf/eliminate-other-ops (:b this)))))
