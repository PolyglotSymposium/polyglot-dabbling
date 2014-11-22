(ns metalang.core)

(defmacro metalang [code]
  `(quote ~code))

(defn comma-sep [values]
  (clojure.string/join ", " values))

(defn third [items]
  (nth items 2))

(def anon? #(or (= 'anon %) (= 'λ %)))
(def define? #(= 'define %))
(def return? #(= 'return %))
(def call? #(= 'call %))

(defn translate-js [code]
  (if (not (list? code))
    code
    (let [first (first code)
          second #(second code)
          third #(third code)]
      (cond
        (anon? first)
          "function () { }"
        (return? first)
          (str "return " (translate-js (second)))
        (define? first)
          (str "var answer = " (translate-js (third)))))))

(defn translate-ruby [code]
  (if (not (list? code))
    code
    (let [first (first code)
          second #(second code)
          third #(third code)] 
      (cond
        (anon? first)
          (str "->"
               (let [params (second)]
                 (if (empty? params) "" (str "(" (comma-sep params) ")")))
               "{}")
        (define? first)
          (str (second) " = " (translate-ruby (third)))
        (return? first)
          ((comp str translate-ruby second))
        (call? first)
          (str "(" (translate-ruby (second)) ").(" (comma-sep (third)) ")")))))

(def translator-of {:ruby translate-ruby
                    :javascript translate-js})

(defn translate [lang code]
  ((translator-of lang) code))
