(defn bind [xs f]
  (mapcat f xs))

(def return list)

(println 
  (bind (list 1 2 3)
        (fn [x] (bind (list 4 5 6)
                      (fn [y] (return [x y]))))))

; => ([1 4] [1 5] [1 6] [2 4] [2 5] [2 6] [3 4] [3 5] [3 6])

(def list-monad
  {
  :return list
  :bind (fn [xs f] (mapcat f xs))
  })

(def maybe-monad
  {
  :return (fn [x] [:just x])
  :bind (fn [maybe f]
    (if (nil? maybe)
      nil
      (f (second maybe))))
  })

(defn- expand [m actions]
  (let [a (first actions)
        as (rest actions)]
    (cond
      (not (seq? a)) a
      (= 'return (first a)) (list (list :return m) (expand m (list (second a))))
      (= '<- (second a)) (list (list :bind m) (nth a 2) (list 'fn [(first a)] (expand m as)))
      :else a)))

(defmacro defn-monad [m fn-name args & actions]
  (list 'defn fn-name args
    (if (empty? actions)
      (throw (IllegalArgumentException. "defn-monad must be given actions."))
      (expand m actions))))

(defn-monad list-monad cart-product [xs ys]
  (x <- xs)
  (y <- ys)
  (return [x y]))

; Person looks like ["Name" maybe-father maybe-mother]
(def father second)

(defn-monad maybe-monad fathers-father [person]
  (dad <- (father person))
  (grandpa <- (father dad))
  (return grandpa))

(println "here")
(println (cart-product (list 1 2) (list 4 5)))
(println (fathers-father ["Michael" [:just ["Doug" [:just "Frank"] nil]] nil]))
