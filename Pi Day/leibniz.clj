;;; Implementation of the Leibniz formula for computing Pi.
;;;
;;; https://en.wikipedia.org/wiki/Leibniz_formula_for_%CF%80

(ns leibniz
  (:require [clojure.math.numeric-tower :refer [expt]]))

(defn compute-pi
  "Computes an approximation of Pi using the Leibniz formula.
   The precision of the approximation increases with the
   input `n`, which determines the number of terms in the
   Leibniz series to compute."
  [n]
  (loop [k 0
         sum 0.0]
    (if (= k n)
      (* 4 sum)
      (recur (inc k)
             (+ sum
                (/ (expt -1 k)
                   (inc (* 2 k))))))))

(compute-pi 10000000)
