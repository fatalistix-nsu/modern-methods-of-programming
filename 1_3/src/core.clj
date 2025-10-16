(ns core)

(defn my-map
  [s l]
  (if (= 0 (count s))
    `()
    (let [f (first s)
          r (rest s)]
      (cons (l f) (my-map r l)))))

(defn my-filter
  [s l]
  (if (= 0 (count s))
    `()
    (let [f (first s)
          r (rest s)]
      (if (l f)
        (conj (my-filter r l) f)
        (my-filter r l)))))

(defn -main
  [& args]
  (let [s '("a" "b" "c")
        m (fn [v] (str v v v))
        f (fn [v] (= "a" (str (last v))))]
    (println (my-map s m))))
