(ns ast)

;; =========================================================
;; =                     U T I L I T I E S                 =
;; =========================================================
(defn verify-substitution [k v]
  (when (not (boolean? v))
    (throw (ex-info "value is not a boolean" {:v v})))
  (when (not (keyword? k))
    (throw (ex-info "key is not a keyword" {:k k}))))

;; =========================================================
;; =                         A S T                         =
;; =========================================================
(defprotocol INode
  "General protocol representing basic ast node: only [var, const, and, or, not] are allowed here"
  (substitute [this k v])
  (eq [this other]))

(defrecord True []
  INode
  (substitute [this k v]
    (verify-substitution k v)
    this)

  (eq [_ other] (instance? True other)))

(defrecord False []
  INode
  (substitute [this k v]
    (verify-substitution k v)
    this)

  (eq [_ other] (instance? False other)))

(defrecord Var [name]
  INode
  (substitute [this k v]
    (verify-substitution k v)
    (if (= k (:name this))
      (if v
        (->True)
        (->False))
      this))

  (eq [_ other]
    (and
      (instance? Var other)
      (= name (:name other)))))

(defrecord And [a b]
  INode
  (substitute [this k v]
    (verify-substitution k v)
    (->And
      (substitute (:a this) k v)
      (substitute (:b this) k v)))

  (eq [_ other]
    (and
      (instance? And other)
      (or
        (and
          (eq a (:a other))
          (eq b (:b other)))
        (and
          (eq a (:b other))
          (eq b (:a other)))))))

(defrecord Or [a b]
  INode
  (substitute [this k v]
    (verify-substitution k v)
    (->Or
      (substitute (:a this) k v)
      (substitute (:b this) k v)))

  (eq [_ other]
    (and
      (instance? Or other)
      (or
        (and
          (eq a (:a other))
          (eq b (:b other)))
        (and
          (eq a (:b other))
          (eq b (:a other)))))))

(defrecord Not [a]
  INode
  (substitute [this k v]
    (verify-substitution k v)
    (->Not
      (substitute (:a this) k v)))
  (eq [_ other]
    (and
      (instance? Not other)
      (eq a (:a other)))))

;; =========================================================
;; =                   C O N S T R U C T O R S             =
;; =========================================================
(defn make-var [name]
  "Utility constructor var that checks that name is a keyword"
  (when (not (keyword? name))
    (throw (ex-info "name is not a keyword" {:name name})))
  (->Var name))
