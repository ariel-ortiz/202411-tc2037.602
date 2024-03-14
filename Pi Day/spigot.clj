;;; Jerry Gibbons' spigot algorithm to compute an arbitrary amount
;;; of decimal digits of Pi.
;;;
;;; http://www.cs.ox.ac.uk/people/jeremy.gibbons/publications/spigot.pdf

(ns spigot)

; Creates an infinite lazy sequence with the decimal digits of Pi.
(def pi
  (->> [1 0 1 1 3 3 0]
       (iterate
         (fn [[q r t k n l x]]
           (if (< (-' (+' (*' 4 q) r) t) (*' n t))
             [(*' 10 q) (*' 10 (-' r (*' n t))) t k
              (-' (quot (*' 10 (+' (*' 3 q) r)) t) (*' 10 n))
              l (int n)]
             (recur [(*' q k) (*' (+' (*' 2 q) r) l) (*' t l) (inc k)
                     (quot (+' (*' q (+' (*' 7 k) 2)) (*' r l)) (*' t l))
                     (+' l 2) x]))))
       rest
       (map last)))

(take 5 pi)
