(ns xray.core
  (:use clojure.tools.trace))


;; New version
(defmulti parse-item (fn [form]
                       (cond
                        (seq? form) :seq
                        (map? form) :map
                        (vector? form) :vector
                        (set? form) :set)))

(defmethod parse-item :seq
  [form]
  (parse-sexp form))

(defmethod parse-item :map
  [form]
  (into (if (sorted? form) (sorted-map) {})
                     (map #(parse-item %1) form)))

(defmethod parse-item :vector
  [form]
  (vec (map #(parse-item %1) form)))

(defmethod parse-item :set
  [form]
  (into (if (sorted? form) (sorted-set) #{})
                     (map #(parse-item %1) form)))

(defmethod parse-item :default
  [form]
  form)


(defmulti parse-sexp (fn [[sym & rest]]
                       sym))


(defmethod parse-sexp 'if
  [[_ test then else]]
  `(if ~(parse-item test)
     (do
       (print "Then")
       ~(parse-item then))
     (do
       (print "Else")
       ~(parse-item else))))

(defmethod parse-sexp 'quote
  [form]
  form)


(defmethod parse-sexp :default
  [form]
  (let [debugged-form (map #(parse-item %) form)
        [f & params] debugged-form]
    `(let [result# ~debugged-form]
       (print (str "Called : " (quote ~f) " with input : " ~@params " which returned " result#))
       result#)))

(defmacro xray [form]
  (parse-item (macroexpand form)))


;; Old version

;; (defn- contains-symb? [elem vect]
;;   (some #{elem} vect))

;; (defn get-form-symbols [form]
;;   (into #{}
;;         (cond (symbol? form) (vector form)
;;               (coll? form) (reduce concat (map get-form-symbols form)))))


;; (defn- debug-symbol? [symb symbols-vec]
;;       (contains-symb? symb symbols-vec))

;; (defn gen-symbol-prints [prefix symbols]
;;   `(do
;;      ~@(map (fn [p-symbol]
;;              `(println (str ~prefix
;;                             " ( "
;;                             (quote ~p-symbol)
;;                             " ) =>"
;;                             (with-out-str (clojure.pprint/pprint ~p-symbol)))))
;;            symbols)))

;; (defn xray-let [[bindings & body-forms] symbols]
;;   `(let
;;        ~(let [bind-pairs (partition 2 bindings)]
;;           (into []
;;                 (reduce
;;                  concat
;;                 (map
;;                   (fn [[bind-name bind-form]]
;;                     (if (debug-symbol? bind-name symbols)
;;                       [bind-name `(let [res# ~(xray bind-form symbols)]
;;                                            (print (str "LET ( " (quote ~bind-name)
;;                                                        " ) =>"
;;                                                        (with-out-str (clojure.pprint/pprint res#))))
;;                                            res#)]
;;                         [bind-name (xray bind-form symbols)]))
;;                   bind-pairs))))
;;      ~@(xray body-forms symbols)))


;; (defn xray-defn [[fn-name params & body-forms] symbols]
;;   `(defn ~fn-name ~params
;;      (println (str "-----------------> [ " (quote ~fn-name) " ]"))
;;      ~(gen-symbol-prints
;;        (name fn-name)
;;        (filter #(debug-symbol? %1 symbols) (get-form-symbols params)))
;;        (let [start-time# (. java.lang.System (clojure.core/nanoTime))
;;              ret# (do ~@body-forms)
;;              retpp# (with-out-str (clojure.pprint/pprint ret#))
;;              end-time# (. java.lang.System (clojure.core/nanoTime))
;;              delta# (/ (double (- end-time# start-time#)) 1000000.0)]
;;             (println (str "<----------------- [ " (quote ~fn-name) " ] =>" retpp#))
;;             (println (str "TIME " (quote ~fn-name) " in " delta# " msecs"))
;;          ret#)))

;; (defn xray-if [[test then else] symbols]
;;   (let [test-debug-symbols (clojure.set/intersection (set (get-form-symbols test))
;;                                                      (set symbols))]
;;     `(do
;;        (println (str "IF " (quote ~test)))
;;        ~(when (not (empty? test-debug-symbols))
;;           (gen-symbol-prints "With IF TEST PARAM " test-debug-symbols))
;;        (if ~test
;;          (do
;;            (println  (str (quote ~test) " is TRUE"))
;;            ~then)
;;        (do
;;          (println  (str (quote ~test) " is FALSE"))
;;          ~else)))))

;; (defn xray-cond [args symbols]
;;   (let [cond-pairs (partition 2 args)
;;         test-debug-symbols (clojure.set/intersection (set (get-form-symbols test))
;;                                                      (set symbols))]
;;     `(do
;;        (cond
;;         ~@(reduce concat
;;                   (map (fn [[cond-test cond-form]]
;;                          `(~cond-test ~(let [test-debug-symbols (clojure.set/intersection (set (get-form-symbols test))
;;                                                                                           (set symbols))]
;;                                          `(do
;;                                             ~(when (not (empty? test-debug-symbols))
;;                                                (gen-symbol-prints "With IF TEST PARAM " test-debug-symbols))
;;                                             (print (str "COND " (quote ~cond-test) " is TRUE"))
;;                                             ~cond-form))))
;;                        cond-pairs))))))

;; (defn xray [form symbols]
;;   (cond
;;    (list? form) (let [f (first form)]
;;                   (cond (and (symbol? f) (= f 'let))
;;                         (xray-let (rest form) symbols)
;;                         (and (symbol? f) (= f 'defn))
;;                         (xray-defn (rest form) symbols)
;;                         (and (symbol? f) (= f 'if))
;;                         (xray-if (rest form) symbols)
;;                         (and (symbol? f) (= f 'cond))
;;                         (xray-cond (rest form) symbols)
;;                         :else
;;                         (map #(xray %1 symbols) form)))
;;    (seq? form) (doall (map #(xray %1 symbols) form))
;;    (vector? form) (vec (map #(xray %1 symbols) form))
;;    (map? form) (into (if (sorted? form) (sorted-map) {})
;;                      (map #(xray %1 symbols) form))
;;    (set? form) (into (if (sorted? form) (sorted-set) #{})
;;                      (map #(xray %1 symbols) form))
;;    :else form))

;; (defmacro xr [symbols form]
;;   (xray form symbols))

;; ;; ------------------------------ TESTS ---------------------

;; (xr [b h j]
;; (let [a 5
;;       b (let [w 5
;;               h (+ w w)]
;;           h)
;;       c (+ a b)]
;;   (let [j 9] j))
;; )

;; (xr [c]
;; (defn defn-test [a [b c] & r]
;;   (+ a b) r)
;; )

;; (defn-test 1 [5 6] '(10))


;; (xr [a]
;;     (let [a 0 b 9]
;;       (if (> a b)
;;         (print "hola")
;;         (print "chau"))))


;; (xr [b]
;;     (let [a 0 b 9]
;;       (cond (> b 5) "Pepe"
;;             (> a 3) "Juan"
;;             :else "Otro")
;;       ))
