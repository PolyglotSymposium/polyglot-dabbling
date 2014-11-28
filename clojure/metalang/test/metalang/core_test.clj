(ns metalang.core-test
  (:require [clojure.test :refer :all]
            [metalang.core :refer :all]))

(deftest metalang-just-returns-its-input-but-quoted
  (is (= '(a b (c)) (metalang (a b (c))))))

(deftest ruby-translate-of-metalang-anon-returns-a-stabby-proc
  (is (= "->{  }" (translate :ruby (metalang (anon [] ()))))))

(deftest javascript-translate-of-metalang-anon-returns-function
  (is (= "function () {  }" (translate :javascript (metalang (anon [] ()))))))

(deftest clojure-translate-of-metalang-anon-returns-fn
  (is (= "(fn [] )" (translate :clojure (metalang (anon [] ()))))))

(deftest ruby-translate-of-metalang-λ-returns-a-stabby-proc
  (is (= "->{  }" (translate :ruby (metalang (λ [] ()))))))

(deftest javascript-translate-of-metalang-λ-returns-function
  (is (= "function () {  }" (translate :javascript (metalang (λ [] ()))))))

(deftest ruby-translate-of-metalang-λ-2
  (is (= "->(a){  }" (translate :ruby (metalang (λ [a] ()))))))

(deftest javascript-translate-of-metalang-λ-2
  (is (= "function (a) {  }" (translate :javascript (metalang (λ [a] ()))))))

(deftest ruby-translate-of-metalang-λ-3
  (is (= "->(a, b){  }" (translate :ruby (metalang (λ [a b] ()))))))

(deftest javascript-translate-of-metalang-λ-3
  (is (= "function (a, b) {  }" (translate :javascript (metalang (λ [a b] ()))))))

(deftest ruby-translate-of-return-is-noop
  (is (= "42" (translate :ruby (metalang (return 42))))))

(deftest javascript-translate-of-return
  (is (= "return 42" (translate :javascript (metalang (return 42))))))

(deftest javascript-translate-of-return-2
  (is (= "return 99" (translate :javascript (metalang (return 99))))))

(deftest javascript-translate-of-define
  (is (= "var answer = 42" (translate :javascript (metalang (define answer 42))))))

(deftest javascript-translate-of-define-2
  (is (= "var answer = 99" (translate :javascript (metalang (define answer 99))))))

(deftest javascript-set
  (is (= "answer = 42" (translate :javascript (metalang (set answer 42))))))

(deftest ruby-set
  (is (= "answer = 42" (translate :ruby (metalang (set answer 42))))))

(deftest ruby-translate-of-define-returns-assignment
  (is (= "answer = 42" (translate :ruby (metalang (define answer 42))))))

(deftest ruby-translate-of-define-2
  (is (= "the_answer = 42" (translate :ruby (metalang (define the_answer 42))))))

(deftest ruby-translate-of-define-3
  (is (= "the_answer = 99" (translate :ruby (metalang (define the_answer 99))))))

(deftest ruby-translate-of-define-4
  (is (= "a = ->{  }" (translate :ruby (metalang (define a (anon [] ())))))))

(deftest ruby-call
  (is (= "(->{  }).()" (translate :ruby (metalang (call (anon [] ()) []))))))

(deftest javascript-call-1
  (is (= "(function () {  })()" (translate :javascript (metalang (call (anon [] ()) []))))))

(deftest ruby-call-2
  (is (= "(->{  }).(a)" (translate :ruby (metalang (call (anon [] ()) [a]))))))

(deftest javascript-call-2
  (is (= "(function () {  })(a, b)" (translate :javascript (metalang (call (anon [] ()) [a b]))))))

(deftest ruby-call-3
  (is (= "(->{  }).(a, b)" (translate :ruby (metalang (call (anon [] ()) [a b]))))))

(deftest js-interesting
  (is (= "var a = function () {  };\nvar b = 42;\n"
         (translate :javascript (metalang [(define a (λ [] ())) (define b 42)])))))

(deftest ruby-interesting
  (is (= "a = ->{  }\nb = 42\n"
         (translate :ruby (metalang [(define a (λ [] ())) (define b 42)])))))

(deftest js-interesting-2
  (is (= "function () { var a = 42;\nreturn a;\n }"
         (translate :javascript (metalang (λ [] [(define a 42) (return a)]))))))

(deftest ruby-interesting-2
  (is (= "->{ a = 42\na\n }"
         (translate :ruby (metalang (λ [] [(define a 42) (return a)]))))))

(deftest many-simple-operators
  (are [exp lang code] (is (= exp (translate lang (metalang code))))
       "(a + b)" :ruby (+ a b)
       "(b + (c + d))" :ruby (+ b (+ c d))
       "(a + b)" :javascript (+ a b)
       "(b + (c + d))" :javascript (+ b (+ c d))
       "(a - b)" :ruby (- a b)
       "(a - b)" :javascript (- a b)
       "(a / b)" :ruby (/ a b)
       "(a / b)" :javascript (/ a b)
       "(a * b)" :ruby (* a b)
       "(a * b)" :javascript (* a b)
       "(a % b)" :ruby (% a b)
       "(a % b)" :javascript (% a b)))
