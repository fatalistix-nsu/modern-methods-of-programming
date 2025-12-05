(ns pfilter)

(defn even-heavy?
  [x]
  (Thread/sleep 10)
  (even? x))

(defn my-partition
  [n coll]
  (when (or (neg? n) (zero? n))
    (throw (ex-info "n must be greater than 0" {:n n})))
  (lazy-seq
    (if-let
      [s (seq coll)]
      (let [chunk (take n s)]
        (cons chunk
              (my-partition n (drop n s))))
      ())))

(defn pfilter
  [pred n coll]
  (when (or (neg? n) (zero? n))
    (throw (ex-info "n must be greater than 0" {:n n})))
  (when-let [s (my-partition n coll)]
    (let [f (pmap (fn [x] (doall (filter pred x))) s)]
      (mapcat identity f))))

(defn -main
  [& args]
  (try
    (time
      (let [x (pfilter even-heavy? 11 (range 0 300 1))]
        (println (doall x))))                                       ; 3118 ms
    (time
      (let [x (filter even-heavy? (range 0 308 1))]
        (println (doall x))))
    (finally
      (shutdown-agents))))
