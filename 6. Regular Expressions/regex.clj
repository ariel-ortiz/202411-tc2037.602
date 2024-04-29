(ns regex
  (:require [clojure.test :refer [deftest is run-tests]]))


;;; Problema 1
;;; Regular expression:
(def c-identifier #"[A-Za-z_]\w*")

(deftest test-c-identifier
  (is (re-matches c-identifier "_"))
  (is (re-matches c-identifier "a"))
  (is (re-matches c-identifier "A"))
  (is (re-matches c-identifier "_an_identfier_42"))
  (is (re-matches c-identifier "_1234567890"))
  (is (re-matches c-identifier "___________"))
  (is (re-matches c-identifier "ThisIsAnIdentfier"))
  (is (not (re-matches c-identifier "")))
  (is (not (re-matches c-identifier "5")))
  (is (not (re-matches c-identifier "1234567890")))
  (is (not (re-matches c-identifier "#!@$^")))
  (is (not (re-matches c-identifier "_a_b_c_$"))))

;;; Problema 2
;;; Regular expression:
(def scheme-boolean #"#(t(rue)?|f(alse)?)")

(deftest test-scheme-boolean
  (is (re-matches scheme-boolean "#t"))
  (is (re-matches scheme-boolean "#f"))
  (is (re-matches scheme-boolean "#true"))
  (is (re-matches scheme-boolean "#false"))
  (is (not (re-matches scheme-boolean "t")))
  (is (not (re-matches scheme-boolean "f")))
  (is (not (re-matches scheme-boolean "true")))
  (is (not (re-matches scheme-boolean "false")))
  (is (not (re-matches scheme-boolean "()")))
  (is (not (re-matches scheme-boolean "0")))
  (is (not (re-matches scheme-boolean "T")))
  (is (not (re-matches scheme-boolean "F")))
  (is (not (re-matches scheme-boolean "#v")))
  (is (not (re-matches scheme-boolean "#truth")))
  (is (not (re-matches scheme-boolean "#falsy"))))

;;; Problema 3
;;; Regular expression:
(def scheme-integer #"\d+|#(b[01]+|d\d+|o[0-7]+|x[0-9a-fA-F]+)")

(deftest test-scheme-integer
  (is (re-matches scheme-integer "0"))
  (is (re-matches scheme-integer "24601"))
  (is (re-matches scheme-integer "#d1234567890"))
  (is (re-matches scheme-integer "#b10"))
  (is (re-matches scheme-integer "#o12345670"))
  (is (re-matches scheme-integer "#x1234567890abcdefABCDEF"))
  (is (not (re-matches scheme-integer "")))
  (is (not (re-matches scheme-integer "#123")))
  (is (not (re-matches scheme-integer "#da1234567890")))
  (is (not (re-matches scheme-integer "#b102")))
  (is (not (re-matches scheme-integer "#o123456780")))
  (is (not (re-matches scheme-integer
                       "#x1234567890abcdefgABCDEF"))))

;;; Problema 6
;;; Regular expression:
(def c-comment #"[/][*](.|\n)*?[*][/]")

(deftest test-c-comment
  (is (re-matches c-comment "/**/"))
  (is (re-matches c-comment "/*-*/"))
  (is (re-matches c-comment "/*\n*/"))
  (is (re-matches c-comment
                  "/***********
                   /*         *
                   /*         *
                   /***********/"))
  (is (= 3 (count (re-seq c-comment "/*********
                                      Comment 1
                                      *********/

                                     /*********
                                      Comment 2
                                      *********/

                                     /*********
                                      Comment 3
                                      *********/"))))
  (is (not (re-matches c-comment "/")))
  (is (not (re-matches c-comment "/*")))
  (is (not (re-matches c-comment "/**")))
  (is (not (re-matches c-comment "/*/")))
  (is (not (re-matches c-comment "//")))
  (is (not (re-matches c-comment "/** /")))
  (is (not (re-matches c-comment "******/")))
  (is (not (re-matches c-comment "/ * * * */"))))


(run-tests)
