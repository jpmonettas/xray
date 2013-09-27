(ns xray.core
  (:use clojure.walk))


(defn- symbol-is-marked? [sym]
  (= (get (name sym) 0) \?))

(defn- remove-mark-from-symbol [sym]
  (symbol (subs (name sym) 1)))

(defn- remove-all-symbol-marks [vect-tree]
  (postwalk 
   (fn [form]
     (if (and (symbol? form) (symbol-is-marked? form))
       (remove-mark-from-symbol form)
       form))
   vect-tree))

(defn- question-symbol-mark-replace [body pr-func]
  (postwalk
   (fn [form]
     (if (and (symbol? form) (symbol-is-marked? form))
       (let [sym# (remove-mark-from-symbol form)]
         `(do
            (~pr-func (str (quote ~sym#)
                           "=>"
                           (with-out-str (clojure.pprint/pprint ~sym#))))
            ~sym#))
       form))
   body))

(defn- if-replace [pr-func args]
  (let [[test then else] args
        debugged-test (question-symbol-mark-replace test pr-func)]
    `(do
       (~pr-func (str "IF " (quote ~test)))
       (if ~debugged-test
         (do
           (~pr-func  (str (quote ~test) " is TRUE"))
           ~then)
       (do
         (~pr-func  (str (quote ~test) " is FALSE"))
         ~else)))))

(defn- cond-replace [pr-func args]
  (let [cond-pairs (partition 2 args)]
    `(do
       (~pr-func "COND")
       (cond 
        ~@(reduce concat
                  (map (fn [[cond-test cond-form]]
                         (let [debugged-test (question-symbol-mark-replace cond-test pr-func)]
                           `(~debugged-test (do
                                              (~pr-func (str (quote ~cond-test) " is TRUE"))
                                              ~cond-form))))
                  cond-pairs))))))


(defn- let-replace [pr-func args]
  (let [[bindings & body-forms] args]
    `(let 
         ~(let [bind-pairs (partition 2 bindings)]
          (into []
                (reduce 
                 concat
                 (map 
                  (fn [[bind-name bind-form]]
                    (if (symbol-is-marked? bind-name) 
                      (let [bind-name-symbol (remove-mark-from-symbol bind-name)]
                        [bind-name-symbol `(let [res# ~bind-form]
                                             (~pr-func (str "LET " (quote ~bind-name-symbol)
                                                            "=>"
                                                            (with-out-str (clojure.pprint/pprint res#))))
                                             res#)])
                      [bind-name bind-form]))
                  bind-pairs))))
     ~@body-forms)))


(defn- flat-vector-tree [v-tree]
  (into []
        (if (vector? v-tree)
          (reduce concat (map #(flat-vector-tree %) v-tree))
          (vector v-tree))))

(defn- defn-replace [pr-funct [fn-name params & body-forms]]
  (let [clean-params (remove-all-symbol-marks params)]
    `(defn ~fn-name ~clean-params
       (~pr-funct (str "-> FUNC " (quote ~fn-name)))
       ~@(map (fn [p-symbol]
                `(~pr-funct (str (quote ~fn-name) 
                                 " param "
                                 (quote ~p-symbol)
                                 "=>"
                                 (with-out-str (clojure.pprint/pprint ~p-symbol)))))
              (remove-all-symbol-marks
               (filter 
                symbol-is-marked? 
                (flat-vector-tree params))))
       (let [start-time# (. java.lang.System (clojure.core/nanoTime))
             ret# ~@body-forms
             retpp# (with-out-str (clojure.pprint/pprint ret#))
             end-time# (. java.lang.System (clojure.core/nanoTime))
             delta# (/ (double (- end-time# start-time#)) 1000000.0)]
         (~pr-funct (str "<- " 
                         (quote ~fn-name)
                         " ret=>" retpp# " in " delta# " msecs"))
         ret#))))
  
(defmacro xray
  [pr-func body]
  (postwalk 
   (fn [form]
     (cond  (coll? form)
            (let [f (first form)
                  args (rest form)]
              (if (symbol? f)
                (cond (= f 'if)
                      (if-replace pr-func args)
                      (= f 'cond)
                      (cond-replace pr-func args)
                      (= f 'let)
                      (let-replace pr-func args)
                      (= f 'defn)
                      (defn-replace pr-func args)
                      :else
                      form)
                form))
            :else     
            form))
   body))


