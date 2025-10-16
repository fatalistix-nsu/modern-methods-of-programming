(ns core)

(defn create-word-seq
  [s alphabet result]
  (if (= 0 (count alphabet))
    result
    (let [f (first alphabet)
          r (rest alphabet)
          new-result (conj result (str s f))]
      (if (= 0 (count s))
        (create-word-seq s r new-result)
        (if (= f (str (last s)))
          (create-word-seq s r result)
          (create-word-seq s r new-result))))))

(defn add-symbol
  [s alphabet result n]
  (if (= 0 (count s))
    result
    (let [f (first s)
          r (rest s)]
      (if (= n (count f))
        (add-symbol r alphabet (conj result f) n)
        (add-symbol (concat r (create-word-seq f alphabet `())) alphabet result n)))))

(defn -main
  [& args]
  (let [a '("a" "b" "c")
        n 2]
    (println (add-symbol `("") a `() n))))
