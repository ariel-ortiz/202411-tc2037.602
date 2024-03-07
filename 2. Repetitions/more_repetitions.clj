(ns more-repetitions
  (:require [clojure.test :refer [deftest is run-tests]]))

; Problema 1
(defn expand
  [s]
  (mapcat repeat
          (range 1 (inc (count s)))
          s))

; Problema 2
;(defn insert
;  [n s]
;  (cond
;    (empty? s)       (list n)
;    (<= n (first s)) (cons n s)
;    :else            (cons (first s)
;                           (insert n (rest s)))))

; Problema 2
(defn insert
  [n s]
  (concat (take-while #(< % n) s)
          (list n)
          (drop-while #(< % n) s)))

; Problema 3 (versión recursiva)
;(defn insertion-sort
;  [s]
;  (if (empty? s)
;    ()
;    (insert (first s)
;            (insertion-sort (rest s)))))

; Problema 3 (versión loop/recur)
;(defn insertion-sort
;  [s]
;  (loop [s s
;         r ()]
;    (if (empty? s)
;      r
;      (recur (rest s)
;             (insert (first s) r)))))

; Problema 3 (versión API de secuencias)
(defn insertion-sort
  [s]
  (reduce #(insert %2 %1) () s))

; Problema 5
(defn binary
  [n]
  (loop [n n
         r ()]
    (if (zero? n)
      r
      (recur (quot n 2)
             (cons (rem n 2) r)))))

; Problema 6
(defn prime-factors
  [n]
  (loop [n n
         r []
         i 2]
    (cond
      (= n 1)            (concat r ())

      (zero? (rem n i))  (recur (quot n i)
                                (conj r i)
                                i)

      :else              (recur n
                                r
                                (inc i)))))

; Problema 10
(defn pack
  [s]
  (partition-by identity s))

(deftest test-expand
  (is (= () (expand ())))
  (is (= '(a) (expand '(a))))
  (is (= '(1 2 2 3 3 3 4 4 4 4) (expand '(1 2 3 4))))
  (is (= '(a b b c c c d d d d e e e e e)
         (expand '(a b c d e)))))

(deftest test-insert
  (is (= '(14) (insert 14 ())))
  (is (= '(4 5 6 7 8) (insert 4 '(5 6 7 8))))
  (is (= '(1 3 5 6 7 9 16) (insert 5 '(1 3 6 7 9 16))))
  (is (= '(1 5 6 10) (insert 10 '(1 5 6)))))

(deftest test-insertion-sort
  (is (= () (insertion-sort ())))
  (is (= '(0 1 3 3 4 6 7 8 9)
         (insertion-sort '(4 3 6 8 3 0 9 1 7))))
  (is (= '(1 2 3 4 5 6) (insertion-sort '(1 2 3 4 5 6))))
  (is (= '(1 5 5 5 5 5 5) (insertion-sort '(5 5 5 1 5 5 5)))))

(deftest test-binary
  (is (= () (binary 0)))
  (is (= '(1 1 1 1 0) (binary 30)))
  (is (= '(1 0 1 1 0 0 0 0 0 1 0 0 0 0 1 1) (binary 45123))))

(deftest test-prime-factors
  (is (= () (prime-factors 1)))
  (is (= '(2 3) (prime-factors 6)))
  (is (= '(2 2 2 2 2 3) (prime-factors 96)))
  (is (= '(97) (prime-factors 97)))
  (is (= '(2 3 3 37) (prime-factors 666))))

(deftest test-pack
  (is (= () (pack ())))
  (is (= '((a a a a) (b) (c c) (a a) (d) (e e e e))
         (pack '(a a a a b c c a a d e e e e))))
  (is (= '((1) (2) (3) (4) (5)) (pack '(1 2 3 4 5))))
  (is (= '((9 9 9 9 9 9 9 9 9)) (pack '(9 9 9 9 9 9 9 9 9)))))

(run-tests)
