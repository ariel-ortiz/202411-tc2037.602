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

(run-tests)
