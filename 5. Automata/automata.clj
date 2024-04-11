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

(run-tests)
