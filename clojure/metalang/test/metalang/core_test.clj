(ns metalang.core-test
  (:require [clojure.test :refer :all]
            [metalang.core :refer :all]))

(deftest metalang-just-returns-its-input-but-quoted
  (is (= '(a b (c)) (metalang (a b (c))))))

(deftest ruby-translate-of-metalang-anon-returns-a-stabby-proc
  (is (= "-> {}" (translate :ruby (metalang (anon [] ()))))))
