(ns core
  (:require [ast])
  (:require [dnf])
  (:require [ext]))

(defn -main
  []
  "((x or y) and 1) or (x and not y)"
  (let [i (ast/->Or
            (ast/->And
              (ast/->Or
                (ast/make-var :x)
                (ast/make-var :y))
              (ast/->True))
            (ast/->And
              (ast/make-var :x)
              (ast/->Not (ast/make-var :y))))

        r (dnf/convert i)]
    (println r)
    (println (ast/substitute r :x true))
    (println (ast/substitute r :y true))
    (println (-> i
                 (ast/substitute :x false)
                 (ast/substitute :y false)
                 (dnf/convert)))
    ("(((x and 1) or (y and 1)) or (x and (not y))")))