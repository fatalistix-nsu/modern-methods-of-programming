(ns dnf
  (:require [ast])
  (:import (ast And False Not Or True Var)))

(defprotocol ICalcNode
  "Node that can be calculated when all constants are present"
  (calc [this])
  (calc? [this]))

(extend-type True
  ICalcNode
  (calc [_] true)
  (calc? [_] true))

(extend-type False
  ICalcNode
  (calc [_] false)
  (calc? [_] true))

(extend-type Var
  ICalcNode
  (calc [this] (ex-info "unable to calc unassigned var" {:name (:name this)}))
  (calc? [_] false))

(extend-type And
  ICalcNode
  (calc [this]
    (let [a (calc (:a this))
          b (calc (:b this))]
      (and a b)))

  (calc? [this]
    (let [a (calc? (:a this))
          b (calc? (:b this))]
      (and a b))))

(extend-type Or
  ICalcNode
  (calc [this]
    (let [a (calc (:a this))
          b (calc (:b this))]
      (or a b)))

  (calc? [this]
    (let [a (calc? (:a this))
          b (calc? (:b this))]
      (and a b))))

(extend-type Not
  ICalcNode
  (calc [this]
    (let [a (calc (:a this))]
      (not a)))

  (calc? [this]
    (calc? (:a this))))

(defprotocol IDnfEliminateOtherOps
  "The protocol that must be extended by extension ops. Extension ops must be converted to basic ast ops"
  (eliminate-other-ops [this]))

(defprotocol IDnfConvertNode
  "The protocol for converting basic ast nodes to DNF form"
  (push-negations [this])
  (remove-double-negations [this])
  (distribute [this])
  (remove-duplicate-literals [this]))

(extend-type True
  IDnfEliminateOtherOps
  (eliminate-other-ops [this] this)
  IDnfConvertNode
  (push-negations [this] this)
  (remove-double-negations [this] this)
  (distribute [this] this)
  (remove-duplicate-literals [this] this))

(extend-type False
  IDnfEliminateOtherOps
  (eliminate-other-ops [this] this)
  IDnfConvertNode
  (push-negations [this] this)
  (remove-double-negations [this] this)
  (distribute [this] this)
  (remove-duplicate-literals [this] this))

(extend-type Var
  IDnfEliminateOtherOps
  (eliminate-other-ops [this] this)
  IDnfConvertNode
  (push-negations [this] this)
  (remove-double-negations [this] this)
  (distribute [this] this)
  (remove-duplicate-literals [this] this))

(extend-type And
  IDnfEliminateOtherOps
  (eliminate-other-ops [this]
    (ast/->And
      (eliminate-other-ops (:a this))
      (eliminate-other-ops (:b this))))

  IDnfConvertNode
  (push-negations [this]
    (ast/->And
      (push-negations (:a this))
      (push-negations (:b this))))

  (remove-double-negations [this]
    (ast/->And
      (remove-double-negations (:a this))
      (remove-double-negations (:b this))))

  (distribute [this]
    (cond
      (instance? Or (:a this))
      (let [l (:a this)
            r (:b this)]
        (ast/->Or
          (distribute (ast/->And (:a l) r))
          (distribute (ast/->And (:b l) r))))

      (instance? Or (:b this))
      (let [l (:a this)
            r (:b this)]
        (ast/->Or
          (distribute (ast/->And l (:a r)))
          (distribute (ast/->And l (:b r)))))

      :else (ast/->And
              (distribute (:a this))
              (distribute (:b this)))))

  (remove-duplicate-literals [this]
    (cond
      (ast/eq (:a this) (:b this))
      (:a this)

      (and
        (instance? Not (:a this))
        (let [a-n (:a this)
              under-a-n (:a a-n)]
          (ast/eq under-a-n (:b this))))
      (ast/->False)

      (and
        (instance? Not (:b this))
        (let [b-n (:b this)
              under-b-n (:a b-n)]
          (ast/eq under-b-n (:a this))))
      (ast/->False)

      :else this)))

(extend-type Or
  IDnfEliminateOtherOps
  (eliminate-other-ops [this]
    (ast/->Or
      (eliminate-other-ops (:a this))
      (eliminate-other-ops (:b this))))

  IDnfConvertNode
  (push-negations [this]
    (ast/->Or
      (push-negations (:a this))
      (push-negations (:b this))))

  (remove-double-negations [this]
    (ast/->Or
      (remove-double-negations (:a this))
      (remove-double-negations (:b this))))

  (distribute [this]
    (ast/->Or
      (distribute (:a this))
      (distribute (:b this))))

  (remove-duplicate-literals [this]
    (cond
      (ast/eq (:a this) (:b this))
      (:a this)

      (and
        (instance? Not (:a this))
        (let [a-n (:a this)
              under-a-n (:a a-n)]
          (ast/eq under-a-n (:b this))))
      (ast/->True)

      (and
        (instance? Not (:b this))
        (let [b-n (:b this)
              under-b-n (:a b-n)]
          (ast/eq under-b-n (:a this))))
      (ast/->True)

      :else this)))

(extend-type Not
  IDnfEliminateOtherOps
  (eliminate-other-ops [this]
    (ast/->Not (eliminate-other-ops (:a this))))

  IDnfConvertNode
  (push-negations [this]
    (cond
      (instance? And (:a this))
      (let [a (:a this)]
        (ast/->Or
          (push-negations (ast/->Not (:a a)))
          (push-negations (ast/->Not (:b a)))))

      (instance? Or (:a this))
      (let [o (:a this)]
        (ast/->And
          (push-negations (ast/->Not (:a o)))
          (push-negations (ast/->Not (:b o)))))

      :else (ast/->Not (push-negations (:a this)))))

  (remove-double-negations [this]
    (if (instance? Not (:a this))
      (let [n (:a this)]
        (remove-double-negations (:a n)))
      (ast/->Not (remove-double-negations (:a this)))))

  (distribute [this]
    (ast/->Not (distribute (:a this))))

  (remove-duplicate-literals [this]
    (ast/->Not (remove-duplicate-literals (:a this)))))

(defn convert
  "This functions converts boolean expression to DNF"
  [node]
  (let [r (-> node
              (eliminate-other-ops)
              (push-negations)
              (remove-double-negations)
              (distribute)
              (remove-duplicate-literals))]
    (if (calc? r)
      (calc r)
      r)))
