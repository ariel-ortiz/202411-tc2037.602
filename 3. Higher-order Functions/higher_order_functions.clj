(ns higher-order-functions
  (:require [clojure.test :refer [deftest is run-tests]])
  (:require [clojure.algo.generic.math-functions
             :refer [abs approx=]]))

; Problema 1
(defn argswap
  [f]
  (fn [x y] (f y x)))


(deftest test-argswap
  (is (= '(2 1)
         ((argswap list) 1 2)))
  (is (= -7
         ((argswap -) 10 3)))
  (is (= 1/4
         ((argswap /) 8 2)))
  (is (= '((4 5 6) 1 2 3)
         ((argswap cons) '(1 2 3) '(4 5 6))))
  (is (= '(1 0 4 25 100)
         ((argswap map) '(-1 0 2 5 10) #(* % %)))))

(run-tests)
