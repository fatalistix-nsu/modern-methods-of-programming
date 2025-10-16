(ns core)

(defn my-map
  [l s]
  (if (= 0 (count s))
    `()
    (let [f (first s)
          r (rest s)]
      (cons (l f) (my-map l r)))))

(defn my-filter
  [l s]
  (if (= 0 (count s))
    `()
    (let [f (first s)
          r (rest s)]
      (if (l f)
        (conj (my-filter l r) f)
        (my-filter l r)))))

(defn create-word-seq
  [s alphabet]
  (if (= 0 (count alphabet))
    (my-map (fn [v] (str s v)) alphabet)
    (my-map (fn [v] (str s v)) (my-filter (fn [v] (not (= v (str (last s))))) alphabet))))

(defn add-symbol
  [s alphabet n]
  (if (= n (count s))
    (conj `() s)
    (reduce
      ;(fn [a v] (concat a v))
      #(concat %1 %2)
      (my-map
        (fn [v] (add-symbol v alphabet n))
        (create-word-seq s alphabet)))))

(defn -main
  [& args]
  (let [a '("a" "b" "c")
        n 3]
    (println (add-symbol "" a n))))
