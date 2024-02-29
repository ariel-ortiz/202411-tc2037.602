(ns more-repetitions
  (:require [clojure.test :refer [deftest is run-tests]]))

; Problema 1
(defn expand
  [s]
  (mapcat repeat
          (range 1 (inc (count s)))
          s))

(deftest test-expand
  (is (= () (expand ())))
  (is (= '(a) (expand '(a))))
  (is (= '(1 2 2 3 3 3 4 4 4 4) (expand '(1 2 3 4))))
  (is (= '(a b b c c c d d d d e e e e e)
         (expand '(a b c d e)))))

(run-tests)
