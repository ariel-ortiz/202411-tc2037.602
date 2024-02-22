(ns factorial)

; Factorial versión recursiva
(defn fact-v1
  [n]
  (if (zero? n)
    1
    (*' n (fact-v1 (dec n)))))

; Factorial versión loop/recur (iterativa)
(defn fact-v2
  [n]
  (loop [i 1
         r 1]
    (if (> i n)
      r
      (recur (inc i)
             (*' i r)))))

; Factorial versión API de secuencias
(defn fact-v3
  [n]
  (reduce *' (range 2 (inc n))))

(fact-v3 10000)
