(ns turing-machines
  (:require [clojure.test :refer [deftest is run-tests]])
  (:import (java.io Writer)))

(defrecord TM [initial-state accept-states transitions])

(defrecord Tape [left head right]
  Object
  (toString [_] (format "%s[%s]%s" left head right)))

(defmethod print-method Tape
  [self ^Writer writer]
  (.write writer (str self)))

; (def t (->Tape "aaaa" \b "cccccc"))

; (println t)

(defn make-tape
  ([s]
   (let [s (drop-while #(= % \_) s)]
     (make-tape ""
                (if (empty? s) \_ (first s))
                (rest s))))
  ([left head right]
   (let [left (drop-while #(= % \_) left)
         right (reverse (drop-while #(= % \_) (reverse right)))]
     (->Tape (apply str left)
             head
             (apply str right)))))

(defn write-tape
  [{:keys [left right]} value]
  (make-tape left value right))

(defn shift-head
  [{:keys [left head right]} direction]
  (case direction
    :right (make-tape (str left head)
                      (or (first right) \_)
                      (rest right))
    :left (make-tape (or (butlast left) ())
                     (or (last left) \_)
                     (str head right))
    (throw (ex-info (str "Bad direction: " direction) {}))))

(defn accepts
  [{:keys [initial-state accept-states transitions]} input]
  (loop [tape (make-tape input)
         current-state initial-state]
    (if (contains? accept-states current-state)
      (str tape)
      (if-let [[write-symbol direction new-state]
               ((transitions current-state) (.head tape))]
        (recur (shift-head (write-tape tape write-symbol)
                           direction)
               new-state)
        nil))))

; Problema 1
(def tm-1 (->TM :q0
                #{:q2}
                {:q0 {\a [\a :right :q1]
                      \_ [\_ :left :q2]}
                 :q1 {\a [\a :right :q0]}}))

(deftest test-problem1
  (is (= "[_]"
         (accepts tm-1 "")))
  (is (= "a[a]"
         (accepts tm-1 "aa")))
  (is (= "aaaaaaa[a]"
         (accepts tm-1 "aaaaaaaa")))
  (is (= "aaaaaaaaaaaaaaaaaaaaaaaaa[a]"
         (accepts tm-1 "aaaaaaaaaaaaaaaaaaaaaaaaaa")))
  (is (nil? (accepts tm-1 "a")))
  (is (nil? (accepts tm-1 "aaa")))
  (is (nil? (accepts tm-1 "aaaaaaa")))
  (is (nil? (accepts tm-1 "aaaaaaaaaaaaaaaaaaaaaaaaa"))))

; Problema 2
(def tm-2 (->TM :q0
                #{:q3}
                {:q0 {\0 [\0 :right :q1]
                      \1 [\1 :right :q2]}
                 :q1 {\1 [\1 :right :q1]
                      \_ [\_ :left :q3]}
                 :q2 {\0 [\0 :right :q2]
                      \_ [\_ :left :q3]}}))

(deftest test-problem2
  (is (= "[0]"
         (accepts tm-2 "0")))
  (is (= "[1]"
         (accepts tm-2 "1")))
  (is (= "1[0]"
         (accepts tm-2 "10")))
  (is (= "0111111111[1]"
         (accepts tm-2 "01111111111")))
  (is (nil? (accepts tm-2 "")))
  (is (nil? (accepts tm-2 "00")))
  (is (nil? (accepts tm-2 "100000000001")))
  (is (nil? (accepts tm-2 "10011010100101011"))))

; Problema 3
(def tm-3 (->TM :q0
                #{:q3}
                {:q0 {\0 [\0 :right :q0]
                      \1 [\1 :right :q0]
                      \_ [\_ :left :q1]}
                 :q1 {\0 [\1 :right :q2]
                      \1 [\0 :left :q1]
                      \_ [\1 :right :q2]}
                 :q2 {\0 [\0 :right :q2]
                      \_ [\_ :left :q3]}}))

(deftest test-problem3
  (is (= "[1]"
         (accepts tm-3 "0")))
  (is (= "1[0]"
         (accepts tm-3 "1")))
  (is (= "1[1]"
         (accepts tm-3 "10")))
  (is (= "10[0]"
         (accepts tm-3 "11")))
  (is (= "100[1]"
         (accepts tm-3 "1000")))
  (is (= "10101011[0]"
         (accepts tm-3 "101010101")))
  (is (= "000000000[1]"
         (accepts tm-3 "0000000000")))
  (is (= "11111000[0]"
         (accepts tm-3 "111101111")))
  (is (= "101001101[1]"
         (accepts tm-3 "1010011010")))
  (is (= "1000000000000000[0]"
         (accepts tm-3 "1111111111111111"))))

; Problema 4
(def tm-4 (->TM :q0
                #{:q4}
                {:q0 {\a [\a :right :q0]
                      \$ [\$ :right :q0]
                      \_ [\_ :left :q1]}
                 :q1 {\a [\_ :left :q2]
                      \$ [\_ :left :q4]}
                 :q2 {\a [\a :left :q2]
                      \$ [\$ :left :q2]
                      \_ [\_ :right :q3]}
                 :q3 {\a [\_ :right :q0]
                      \$ [\_ :right :q5]}
                 :q5 {\a [\_ :right :q5]
                      \_ [\_ :left :q4]}}))

(deftest test-problem4
  (is (= "[_]"
         (accepts tm-4 "$")))
  (is (= "[_]"
         (accepts tm-4 "a$a")))
  (is (= "[a]"
         (accepts tm-4 "aa$a")))
  (is (= "aa[a]"
         (accepts tm-4 "aaaaa$aa")))
  (is (= "[_]"
         (accepts tm-4 "aaaaa$aaaaaaaa")))
  (is (= "aa[a]"
         (accepts tm-4 "aaaaaaaa$aaaaa")))
  (is (= "[_]"
         (accepts tm-4 "$aaaaaaaaaaaaa")))
  (is (= "aaaaaaaaaaaa[a]"
         (accepts tm-4 "aaaaaaaaaaaaa$"))))

(run-tests)
