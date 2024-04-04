(ns parallelism)

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

(time (fact-par n))
