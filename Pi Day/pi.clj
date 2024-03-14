;;; Print Clojure's predefined PI constant.

(ns pi
  (:require [clojure.math :as math]))

(println math/PI)
