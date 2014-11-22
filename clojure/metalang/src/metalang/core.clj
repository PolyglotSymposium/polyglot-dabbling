(ns metalang.core)

(defmacro metalang [code]
  `(quote ~code))

(defn comma-sep [values]
  (clojure.string/join ", " values))

(defn translate-ruby [code]
  (if (not (list? code))
    code
    (let [f (first code)] 
      (cond
        (or (= 'anon f) (= 'λ f))
          (str "->"
               (let [params (nth code 1)]
                 (if (empty? params) "" (str "(" (comma-sep params) ")")))
               "{}")
        (= 'define f)
          (str (nth code 1) " = " (translate-ruby (nth code 2)))
        (= 'call f)
          (str "(" (translate-ruby (nth code 1)) ").(" (comma-sep (nth code 2)) ")")))))

(def translator-of { :ruby translate-ruby })

(defn translate [lang code]
  ((translator-of lang) code))
