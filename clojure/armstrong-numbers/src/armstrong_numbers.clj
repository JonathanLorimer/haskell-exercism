(ns armstrong-numbers
  (:require [clojure.math.numeric-tower :as math]))

(defn digits [n]
  (if (zero? n)
    nil
    (concat
      (digits (math/round (math/floor (/ n 10))))
      (list* (mod n 10) nil)
    )
  )
)

(defn exp [n x]
  (reduce * (repeat n x)))

(defn armstrong? [num]
  (let [d (digits num)]
    (== num (reduce + (map (partial exp (count d)) d)))))
