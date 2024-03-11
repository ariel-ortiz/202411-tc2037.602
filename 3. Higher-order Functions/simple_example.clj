(ns simple-example)

(defn $map
  [fun s]
  (if (empty? s)
    ()
    (cons (fun (first s))
          ($map fun (rest s)))))

($map (fn [x] (* x 2)) [3 -10 0 5])

(defn composite
  [f g]
  (fn [x]
    (f (g x))))

(defn f1 [x] (* x x))
(defn f2 [x] (+ x 3))

(f1 1)
(f2 1)

(def f3 (composite f1 f2))

(f3 1)

(def f4 (composite f2 f1))

(f4 1)

(def f5 (composite f3 f4))

(f5 1)

(def f6 (composite f4 f3))

(f6 1)
