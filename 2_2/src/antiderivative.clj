(ns antiderivative)

(defn closest-index
  [x h]
  (int (Math/floor (/ x h))))

(defn trapezoid-integral
  [h a b]
  (* (/ h 2) (+ a b)))

(defn antiderivative
  [func h]
  (when (neg? h)
    (throw (ex-info "h must be positive" {:h h})))

  (let [trapezoid-integral-h (partial trapezoid-integral h)
        f (->> 0
               (iterate #(+ h %))
               (map func))
        r (->> f
               (#(map trapezoid-integral-h % (rest %)))
               (reductions +)
               (cons 0.0))]
    (fn
      [x]
      (when (neg? x)
        (throw (ex-info "x must be positive" {:x x})))

      (let [n (closest-index x h)]
        (let [cached (nth r n)
              a (* n h)]
          (if (== a x)
            cached
            (+ cached (trapezoid-integral (- x a) (nth f n) (func x)))))))))

(defn -main
  [& args]
  (let [x (iterate #(+ 0.5 %) 0)]
    (println 1)))