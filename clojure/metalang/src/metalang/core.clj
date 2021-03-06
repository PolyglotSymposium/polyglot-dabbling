(ns metalang.core)

(defmacro metalang [code]
  `(quote ~code))

(def comma-sep #(clojure.string/join ", " %))
(def semi-nl-sep #(clojure.string/join ";\n" %))
(def nl-sep #(clojure.string/join "\n" %))

(defn third [items]
  (nth items 2))

(defn infix? [kwd]
  (contains? #{'+ '- '/ '* '%} kwd))

(def anon? #(or (= 'anon %) (= 'λ %)))
(def define? #(= 'define %))
(def -set? #(= 'set %))
(def return? #(= 'return %))
(def call? #(= 'call %))

(defn translate-js [code]
  (if (not (list? code))
    (if (vector? code)
      (str (semi-nl-sep (map translate-js code)) ";\n")
      code)
    (let [first (first code)
          second #(second code)
          third #(third code)]
      (cond
        (infix? first)
          (str "(" (translate-js (second)) " " first " " (translate-js (third)) ")")
        (anon? first)
          (str "function (" (comma-sep (second)) ") { " (translate-js (third)) " }")
        (return? first)
          (str "return " (translate-js (second)))
        (call? first)
          (str "(" (translate-js (second)) ")(" (comma-sep (third)) ")")
        (-set? first)
          (str (second) " = " (translate-js (third)))
        (define? first)
          (str "var " (second) " = " (translate-js (third)))))))

(defn translate-clj [code]
  "(fn [] )")

(defn translate-ruby [code]
  (if (not (list? code))
    (if (vector? code)
      (str (nl-sep (map translate-ruby code)) "\n")
      code)
    (let [first (first code)
          second #(second code)
          third #(third code)] 
      (cond
        (infix? first)
          (str "(" (translate-ruby (second)) " " first " " (translate-ruby (third)) ")")
        (anon? first)
          (str "->"
               (let [params (second)]
                 (if (empty? params) "" (str "(" (comma-sep params) ")")))
               "{ " (translate-ruby (third)) " }")
        (or (-set? first) (define? first))
          (str (second) " = " (translate-ruby (third)))
        (return? first)
          ((comp str translate-ruby second))
        (call? first)
          (str "(" (translate-ruby (second)) ").(" (comma-sep (third)) ")")))))

(def translator-of {:ruby translate-ruby
                    :javascript translate-js
                    :clojure translate-clj})

(defn translate [lang code]
  ((translator-of lang) code))
