(ns english-grammar
  (:require [instaparse.core :refer [parser]])
  (:import (instaparse.gll Failure)))

(defn fails? [r] (instance? Failure r))
(defn succeeds? [r] (not (fails? r)))

(def simple-english-grammar
  (parser "
    sentence = noun-phrase verb-phrase
    noun-phrase = article noun
    verb-phrase = verb noun-phrase
    article = space ('the' | 'a') space
    noun = space ('man' | 'woman' | 'ball' | 'table') space
    verb = space ('hit' | 'took' | 'saw' | 'liked') space
    <space> = <#'\\s*'>
  "))

(let [result (simple-english-grammar "the woman saw a man")]
  (if (succeeds? result)
    (clojure.pprint/pprint result)
    (println result)))
