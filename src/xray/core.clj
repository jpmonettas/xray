(ns xray.core
  (:use clojure.walk))


(defn symbol-is-marked? [sym]
  (= (get (name sym) 0) \?))

(defn remove-mark-from-symbol [sym]
  (symbol (subs (name sym) 1)))

(defn question-symbol-mark-replace [body pr-func]
  (postwalk
   (fn [form]
     (if (and (symbol? form) (symbol-is-marked? form))
       (let [sym# (remove-mark-from-symbol form)]
         `(do
            (~pr-func (str (quote ~sym#) "=>" ~sym#))
            ~sym#))
       form))
   body))

(defn if-replace [pr-func args]
  (let [[test then else] args
        debugged-test (question-symbol-mark-replace test pr-func)]
    `(if ~debugged-test
       (do
         (~pr-func  (str "For " (quote ~test) " we took THEN way"))
         ~then)
       (do
         (~pr-func  (str "For " (quote ~test) " we took ELSE way"))
         ~else))))

(defmacro xray [pr-func body]
  (postwalk 
   (fn [form]
     (cond  (coll? form)
            (let [f (first form)
                  args (rest form)]
              (cond (and (symbol? f) (= f 'if))
                    (if-replace pr-func args)
                    :else
                    form))
            :else     
            form))
   body))


(xray println
      (defn other-test [a b c]
        (if (= ?a b)
          5
          (if (= a c) 6 9))))
