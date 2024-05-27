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


; Problema 1
(def tm-1 (->TM :q0
                #{:q2}
                {:q0 {\a [\a :right :q1]}
                 :q1 {\_ [\_ :left :q2]}}))

(run-tests)
