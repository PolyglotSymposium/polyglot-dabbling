(ns metalang.core-test
  (:require [clojure.test :refer :all]
            [metalang.core :refer :all]))

(deftest metalang-just-returns-its-input-but-quoted
  (is (= '(a b (c)) (metalang (a b (c))))))

(deftest ruby-translate-of-metalang-anon-returns-a-stabby-proc
  (is (= "-> {}" (translate :ruby (metalang (anon [] ()))))))

(deftest ruby-translate-of-define-returns-assignment
  (is (= "answer = 42" (translate :ruby (metalang (define answer 42))))))

(deftest ruby-translate-of-define-2
  (is (= "the_answer = 42" (translate :ruby (metalang (define the_answer 42))))))
