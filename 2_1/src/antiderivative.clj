(ns antiderivative)

;(defn calc-antiderivative-step
;  [f a b]
;  (let [h (- b a)]
;    (* (/ h 2) (+ (f a) (f b)))))
;
;(defn index-to-value
;  [a i h]
;  (+ (a (* i h))))
;
;(defn antiderivative-step
;  [func e s x]
;  (if (= 0 (count s))
;    (if (= e x)
;      0
;      (calc-antiderivative-step func e x))
;    (let [f (first s)
;          r (rest s)
;          sum (calc-antiderivative-step func e f)]
;      (+ sum (antiderivative-step func f r x)))))
;
;(defn antiderivative
;  [func]
;  (let [state (atom {:pos []
;                     :neg []
;                     :max 0
;                     :min 0})]
;    (fn
;      [x]
;      (let [rng (range 0 x 0.1)
;            f (first rng)
;            r (rest rng)]
;        (swap! state
;               (fn [s]
;                 ()))
;        (antiderivative-step func f r x)))))
;
;(defn closest-index
;  [x h]
;  (int (Math/floor (/ x h))))
;
;(defn missing-positive-values
;  [func prev a h i n]
;  (if (> i n)
;    []
;    (let [asfi (calc-antiderivative-step func (+ a (* (- i 1) h)) (+ a (* i h)))
;          v (+ asfi prev)
;          nextI (inc i)]
;      (apply list (into [v] (missing-positive-values func v a h nextI n))))))
;
;(defn missing-negative-values
;  [func prev a h i n]
;  (if (< i n)
;    []
;    (let [asfi (calc-antiderivative-step func (+ a (* (+ i 1) h)) (+ a (* i h)))
;          v (+ asfi prev)
;          nextI (dec i)]
;      (apply list (into [v] (missing-negative-values func v a h nextI n))))))
;
;(defn antiderivative
;  [func]
;  (let [h 0.1
;        state (atom {:max             0
;                     :positive-values (apply list [0])
;                     :negative-values (apply list [0])
;                     :min             0})]
;    (fn
;      [x]
;      (let [i (closest-index x h)]
;        (when (> i (:max @state))
;          (swap! state (fn [s]
;                         (let [start (inc (:max s))
;                               last-value (last (:positive-values s))]
;                           (-> s
;                               (assoc :max i)
;                               (assoc :positive-values (concat (:positive-values s) (missing-positive-values func last-value 0 h start i))))))))
;        (when (< i (:min @state))
;          (swap! state (fn [s]
;                         (let [start (dec (:min s))
;                               last-value (last (:negative-values s))]
;                           (-> s
;                               (assoc :min i)
;                               (assoc :negative-values (concat (:negative-values s) (missing-negative-values func last-value 0 h start i))))))))
;        (if (< i 0)
;          (+ (get (:negative-values @state) i) (calc-antiderivative-step func 0 x))
;          (+ (get (:positive-values @state) i) (calc-antiderivative-step func 0 x)))))))
;
;
;(defn -main
;  [& args]
;  (println (missing-negative-values (fn [x] 1), -1 0 1 -2 -4))
;  (let [f (antiderivative (fn [x] 1))]
;    (println (f -2))
;    (println (f -4))))

(defn closest-index
  [x h]
  (int (Math/floor (/ x h))))

(defn trapezoid-integral
  [f a b]
  (let [h (- b a)]
    (* (/ h 2) (+ (f a) (f b)))))

(defn antiderivative
  [func h]
  (when (neg? h)
    (throw (ex-info "h must be positive" {:h h})))

  (let [state (atom {:positive-values [0.0]})
        func (memoize func)
        extend-positive-values (fn [n]
                                 (swap! state
                                        (fn [s]
                                          (loop [s s]
                                            (let [values (:positive-values s)
                                                  i (count values)]
                                              (if (> i n)
                                                s
                                                (let [a (* (dec i) h)
                                                      b (* i h)
                                                      v (+ (last values) (trapezoid-integral func a b))]
                                                  (recur (assoc s :positive-values (conj values v))))))))))]
    (fn
      [x]
      (when (neg? x)
        (throw (ex-info "x must be positive" {:x x})))

      (let [n (closest-index x h)]
        (when (>= n (count (:positive-values @state)))
          (extend-positive-values n))
        (let [cached (nth (:positive-values @state) n)
              a (* n h)]
          (if (== a x)
            cached
            (+ cached (trapezoid-integral func a x))))))))

(defn -main
  [& args]
  ())