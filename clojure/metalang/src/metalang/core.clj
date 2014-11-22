(ns metalang.core)

(defmacro metalang [code]
  `(quote ~code))
