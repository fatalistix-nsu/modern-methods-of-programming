(ns test-util)

(require '[clojure.test :refer [is]])

(defmacro fail
  ([] `(is false))
  ([msg] `(is false ~msg)))
