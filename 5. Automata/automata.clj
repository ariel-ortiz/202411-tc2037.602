(ns automata
  (:require [clojure.test :refer [deftest is run-tests]]))

(defrecord DFA [initial-state
                accept-states
                transitions])

(defn accepts?
  [{:keys [initial-state accept-states transitions]} input]
  (loop [input (seq input)
         current initial-state]
    (if (empty? input)
      (contains? accept-states current)
      (recur (rest input)
             ((transitions current) (first input))))))

; Problema 1
(def dfa-1 (->DFA :q0
                  #{:q2}
                  {:q0 {\a :q1
                        \b :q0}
                   :q1 {\a :q1
                        \b :q2}
                   :q2 {\a :q2
                        \b :q2}}))

(deftest test-problem1
  (is (accepts? dfa-1 "ab"))
  (is (accepts? dfa-1 "abba"))
  (is (accepts? dfa-1 "aaab"))
  (is (accepts? dfa-1 "abbbbbbbbb"))
  (is (not (accepts? dfa-1 "")))
  (is (not (accepts? dfa-1 "a")))
  (is (not (accepts? dfa-1 "baa")))
  (is (not (accepts? dfa-1 "bbba"))))

; Problema 2
(def dfa-2 (->DFA :q0
                  #{:q2}
                  {:q0 {\0 :q1
                        \1 :q3}
                   :q1 {\0 :q1
                        \1 :q2}
                   :q2 {\0 :q1
                        \1 :q2}
                   :q3 {\0 :q3
                        \1 :q3}}))

(deftest test-problem2
  (is (accepts? dfa-2 "01"))
  (is (accepts? dfa-2 "0101"))
  (is (accepts? dfa-2 "01111"))
  (is (accepts? dfa-2 "000001"))
  (is (not (accepts? dfa-2 "")))
  (is (not (accepts? dfa-2 "00")))
  (is (not (accepts? dfa-2 "1001011")))
  (is (not (accepts? dfa-2 "1001010"))))

; Problema 3
(def dfa-3 (->DFA :q0 #{:q3} {:q0 {\x :q0
                                   \y :q1}
                              :q1 {\x :q0
                                   \y :q2}
                              :q2 {\x :q0
                                   \y :q3}
                              :q3 {\x :q3
                                   \y :q3}}))

(deftest test-problem3
  (is (accepts? dfa-3 "yyy"))
  (is (accepts? dfa-3 "xyxyyyx"))
  (is (accepts? dfa-3 "xxxxxyyyyy"))
  (is (accepts? dfa-3 "yyyxxxxyyy"))
  (is (not (accepts? dfa-3 "")))
  (is (not (accepts? dfa-3 "xxx")))
  (is (not (accepts? dfa-3 "yxxyxxy")))
  (is (not (accepts? dfa-3 "xyxyyxyyx"))))

(run-tests)
