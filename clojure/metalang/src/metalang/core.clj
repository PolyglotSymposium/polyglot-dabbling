(ns metalang.core)

(defmacro metalang [code]
  `(quote ~code))

(defn translate [_ code]
  (if ((complement list?) code)
    code
    (let [f (first code)] 
      (cond
        (= 'anon f)
          "-> {}"
        (= 'define f)
          (str (nth code 1) " = " (translate :ruby (nth code 2)))
        (= 'call f)
          (str "(" (translate :ruby (nth code 1)) ").()")))))
