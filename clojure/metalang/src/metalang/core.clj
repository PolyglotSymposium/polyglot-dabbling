(ns metalang.core)

(defmacro metalang [code]
  `(quote ~code))

(defn comma-sep [values]
  (clojure.string/join ", " values))

(defn third [items]
  (nth items 2))

(defn translate-js [code]
  (cond
    (or (= 'anon (first code)) (= 'λ (first code)))
      "function () { }"
    :else "return 42"))

(defn translate-ruby [code]
  (if (not (list? code))
    code
    (let [f (first code)] 
      (cond
        (or (= 'anon f) (= 'λ f))
          (str "->"
               (let [params (second code)]
                 (if (empty? params) "" (str "(" (comma-sep params) ")")))
               "{}")
        (= 'define f)
          (str (second code) " = " (translate-ruby (third code)))
        (= 'return f)
          ((comp str translate-ruby second) code)
        (= 'call f)
          (str "(" (translate-ruby (second code)) ").(" (comma-sep (third code)) ")")))))

(def translator-of {:ruby translate-ruby
                    :javascript translate-js})

(defn translate [lang code]
  ((translator-of lang) code))
