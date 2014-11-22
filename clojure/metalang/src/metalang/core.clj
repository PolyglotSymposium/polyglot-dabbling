(ns metalang.core)

(defmacro metalang [code]
  `(quote ~code))

(defn translate [_ code]
  "-> {}")
