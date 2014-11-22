(ns metalang.core)

(defmacro metalang [code]
  `(quote ~code))

(defn translate [_ code]
  (let [f (first code)] 
    (cond
      (= 'anon f) "-> {}"
      (= 'define f) "answer = 42")))
