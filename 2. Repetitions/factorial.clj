(ns factorial)

; Factorial version recursiva
(defn fact-v1
  [n]
  (if (zero? n)
    1
    (*' n (fact-v1 (dec n)))))

(fact-v1 1000)
