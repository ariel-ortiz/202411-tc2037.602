(ns parallelism)

; Problem 1
;
; n = 100,000
; p = 8
;
; Run #1  T1 = 3257.621292  T8 = 162.088292
; Run #2  T1 = 3247.618667  T8 = 170.734833
; Run #3  T1 = 3261.098292  T8 = 169.953875
; Run #4  T1 = 3247.702833  T8 = 168.220667
; Run #5  T1 = 3242.386416  T8 = 167.049833
; Average T1 = 3251.2855    T8 = 167.609500
;
; Sp = T1/Tp
; S8 = 3251.2855 / 167.609500 = 19.39797

(def n 100000)

(defn bits
  [x]
  (.bitCount (biginteger x)))

(defn fact-seq
  [n]
  (bits (loop [i 2
               r 1]
          (if (> i n)
            r
            (recur (inc i)
                   (*' r i))))))

;(time (fact-seq n))

; We assume that (zero? (rem n p))
(defn fact-ranges
  [n p]
  (partition 2
             1
             (concat (range 1 n (quot n p))
                     [(inc n)])))

(defn fact-partial
  [[start end]]
  (loop [i start
         r 1]
    (if (= i end)
      r
      (recur (inc i)
             (*' r i)))))

(defn fact-par
  [n]
  (let [p (.availableProcessors (Runtime/getRuntime))]
    (bits (reduce *' (pmap fact-partial (fact-ranges n p))))))

; (time (fact-par n))

; Problem 4
;
; n = 200,000
; p = 8
;
; Run #1  T1 = 1857.431833   T8 = 707.253708
; Run #2  T1 = 2090.520333   T8 = 716.935208
; Run #3  T1 = 1923.118333   T8 = 877.306167
; Run #4  T1 = 1906.732541   T8 = 922.369917
; Run #5  T1 = 1876.284042   T8 = 859.608667
; Average T1 = 1930.8174164  T8 = 816.6947333999999
;
; Sp = T1/Tp
; S8 = 1930.8174164 / 816.6947333999999 = 2.3641849732050693
(defn create-random-data
  [n]
  (repeatedly n #(rand-int 1000)))

;(apply <= (create-random-data 100))

(defn insertion-sort
  [s]
  (loop [s s
         r ()]
    (if (empty? s)
      r
      (let [x (first s)
            [before after] (split-with #(< % x) r)]
        (recur (rest s)
               (concat before [x] after))))))

;(apply <= (insertion-sort (create-random-data 1000)))

(defn merge-algorithm
  [a b]
  (loop [a a
         b b
         r []]
    (cond
      (empty? a)
      (concat r b)

      (empty? b)
      (concat r a)

      (< (first a) (first b))
      (recur (rest a)
             b
             (conj r (first a)))

      :else
      (recur a
             (rest b)
             (conj r (first b))))))

; (merge-algorithm [1 3 10 20 34] [2 3 5 10 20 100])

(defn hybrid-sort-seq
  [s]
  (if (< (count s) 100)
    (insertion-sort s)
    (let [[a b] (split-at (quot (count s) 2) s)]
      (merge-algorithm (hybrid-sort-seq a)
                       (hybrid-sort-seq b)))))

(defn hybrid-sort-par
  [s]
  (if (< (count s) 100)
    (insertion-sort s)
    (let [splitted (split-at (quot (count s) 2) s)]
      (apply merge-algorithm
             (pmap hybrid-sort-par splitted)))))

(def n 200000)
(def random-data (create-random-data n))
(apply <= random-data)
(apply <= (time (hybrid-sort-seq random-data)))
(apply <= (time (hybrid-sort-par random-data)))
(apply <= (time (sort random-data)))
