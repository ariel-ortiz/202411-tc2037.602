(ns grammar
  (:require [clojure.test :refer [deftest is run-tests]])
  (:require [instaparse.core :refer [parser]])
  (:import (instaparse.gll Failure)))

(defn fails? [r] (instance? Failure r))
(defn succeeds? [r] (not (fails? r)))

; Problema 1
(def start-and-end (parser "
  S =   '#'
      | '$'
      | '#' T '#'
      | '$' T '$'
  T =   epsilon
      | '$' T
      | '#' T
"))

(deftest test-start-and-end
  (is (succeeds? (start-and-end "$")))
  (is (succeeds? (start-and-end "#")))
  (is (succeeds? (start-and-end "$$")))
  (is (succeeds? (start-and-end "##")))
  (is (succeeds? (start-and-end "$$$$$$$#$$#$$#$##$")))
  (is (succeeds? (start-and-end "#$$$#$$$$#$$$$#$####")))
  (is (fails? (start-and-end "")))
  (is (fails? (start-and-end "$#")))
  (is (fails? (start-and-end "#$")))
  (is (fails? (start-and-end "###$$#$#$$$#$$$####$")))
  (is (fails? (start-and-end "$#$#$#$$$$#$$$#$$$$#$$#")))
  (is (fails? (start-and-end "#######################$"))))

; Problema 2
(def palindrome (parser "
     Q =   epsilon
         | '0'
         | '1'
         | '0' Q '0'
         | '1' Q '1'
"))

(deftest test-palindrome
  (is (succeeds? (palindrome "")))
  (is (succeeds? (palindrome "0")))
  (is (succeeds? (palindrome "1")))
  (is (succeeds? (palindrome "11")))
  (is (succeeds? (palindrome "00")))
  (is (succeeds? (palindrome "010")))
  (is (succeeds? (palindrome "1111111")))
  (is (succeeds? (palindrome "000010000")))
  (is (succeeds? (palindrome "01001110101110010")))
  (is (fails? (palindrome "01")))
  (is (fails? (palindrome "10")))
  (is (fails? (palindrome "1010")))
  (is (fails? (palindrome "10000000")))
  (is (fails? (palindrome "00010001")))
  (is (fails? (palindrome "1010011010")))
  (is (fails? (palindrome "111111111111111111110"))))

; Problema 3
(def balanced-parentheses (parser "
     P =   epsilon
         | '(' P ')' P
"))

(deftest test-balanced-parentheses
  (is (succeeds? (balanced-parentheses "")))
  (is (succeeds? (balanced-parentheses "()")))
  (is (succeeds? (balanced-parentheses "((((()))))")))
  (is (succeeds? (balanced-parentheses "()()()()()")))
  (is (succeeds? (balanced-parentheses
                   "(()()())((())(()()()))(((())))")))
  (is (fails? (balanced-parentheses "(")))
  (is (fails? (balanced-parentheses ")")))
  (is (fails? (balanced-parentheses "((((())))")))
  (is (fails? (balanced-parentheses "))))((((")))
  (is (fails? (balanced-parentheses
                "(()()())((())(()()()))((((())))"))))

(run-tests)
