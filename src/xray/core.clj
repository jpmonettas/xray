(ns xray.core
  (:use clojure.tools.macro)
  (:require
   [loom.graph :as lgr]
   [loom.alg :as lalg]
   [loom.io :as lio]))


(def graph (atom (lgr/weighted-digraph)))

(defn pprint [obj]
  (with-out-str (clojure.pprint/pprint obj)))

(defn gen-uniq-node [node-map]
  (merge node-map {:node-id (str (gensym))}))

(defn add-transformation [v1 v2 edge]
  (swap! graph (fn [gr]
                 (lgr/add-edges gr
                                [v1 v2 edge]))))
;;;;;;;;;;;;;;;;
;; Multhimethods
;;;;;;;;;;;;;;;;

(defmulti parse-item (fn [form ctx]
                       (cond
                        (seq? form) :seq
                        (map? form) :map
                        (vector? form) :vector
                        (set? form) :set)))

(defmulti parse-sexp (fn [[sym & rest] ctx]
                       sym))


;;;;;;;;;;;;;;;;;;;;;;;;;
;; Methods implementation
;;;;;;;;;;;;;;;;;;;;;;;;;

;; (defmethod parse-item :seq
;;   [form ctx]
;;   (let [f (first form)
;;         parent-node (or (:parent-node ctx) (gen-uniq-node {:result-node true}))
;;         this-node (gen-uniq-node (merge {:form (pprint form)}
;;                                                (when (not (= f 'quote))
;;                                                  {:func (pprint (first form))})))
;;         ctx (assoc ctx :parent-node parent-node :this-node this-node)]
;;     (parse-sexp form ctx)))

(defmethod parse-item :seq
  [form ctx]
  (let [f (first form)
        this-node-info {:func (pprint f)
                        :form (pprint form)}
        ctx (assoc ctx :this-node-info this-node-info)]
    (parse-sexp form ctx)))

(defmethod parse-item :map
  [form ctx]
  {:r-form
   (into (if (sorted? form) (sorted-map) {})
         (map #(:r-form (parse-item %1 ctx)) form))})

;; do ctx thing here like in map
(defmethod parse-item :vector
  [form ctx]
  (vec (map #(parse-item %1 ctx) form)))

;; do ctx thing here like in map
(defmethod parse-item :set
  [form ctx]
  (into (if (sorted? form) (sorted-set) #{})
                     (map #(parse-item %1 ctx) form)))

(defmethod parse-item :default
  [form ctx]
  {:r-form
   `(do
      (add-transformation (gen-uniq-node {})
                          ~(:parent-var ctx)
                          ~form)
      ~form)})

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; S-Expressions parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;


;; (defn merge-node-and-update-parent [ctx merge-opts]
;;   (let [merged (merge-with merge ctx {:this-node merge-opts})
;;         swapped (assoc merged :parent-node (:this-node merged))]
;;     swapped))

;; (defmethod parse-sexp 'map
;;   [[_ f col] ctx]
;;   (let [parent-node (:parent-node ctx)
;;         ctx (merge-node-and-update-parent ctx {:type :map
;;                                                 :map-func (pprint f)})
;;         debugged-col (:r-form (parse-item col ctx))]
;;     {:r-form
;;      `(let [result# (map ~f ~debugged-col)]
;;         (add-transformation ~(:this-node ctx) ~parent-node (pprint result#))
;;         result#)}))

;; (defmethod parse-sexp 'reduce
;;   [[_ f col] ctx]
;;   (let [parent-node (:parent-node ctx)
;;         ctx (merge-node-and-update-parent ctx {:type :reduce
;;                                                 :reduce-func (pprint f)})
;;         debugged-col (parse-item col ctx)
;;         debugged-col-form (:r-form debugged-col)]
;;     {:r-form
;;      `(let [result# (reduce ~f ~debugged-col-form)]
;;         (add-transformation ~(:this-node ctx) ~parent-node (pprint result#))
;;         result#)
;;      :recur (:recur debugged-col)}))

;; (defmethod parse-sexp 'filter
;;   [[_ f col] ctx]
;;   (let [parent-node (:parent-node ctx)
;;         ctx (merge-node-and-update-parent ctx {:type :filter
;;                                                 :filter-func (pprint f)})
;;         debugged-col (parse-item col ctx)
;;         debugged-col-form (:r-form debugged-col)]
;;     {:r-form
;;      `(let [result# (filter ~f ~debugged-col-form)]
;;         (add-transformation ~(:this-node ctx) ~parent-node (pprint result#))
;;         result#)
;;      :recur (:recur debugged-col)}))



;; (defmethod parse-sexp 'if
;;   [[_ test then else] ctx]
;;   (let [parent-node (:parent-node ctx)
;;         this-node (gen-uniq-node {:form-func "if"})
;;         ctx (assoc ctx :parent-node this-node)]

;;     (if ~(parse-item test ctx)
;;      (do
;;        (print "Then")
;;        ~(parse-item then ctx))
;;      (do
;;        (print "Else")
;;        ~(parse-item else ctx)))))

(defmethod parse-sexp 'quote
  [form ctx]
  {:r-form
   `(do
      (add-transformation (gen-uniq-node {})
                          ~(:parent-var ctx)
                          ~form)
      ~form)})



;; (def fact
;;   (fn*
;;    ([n]
;;       (if (zero? n)
;;         1
;;         (* n (fact (dec n)))))))

;; ;; should became something like this :


;; (def fact
;;   (fn*
;;    ([n]
;;       ((fn temp-fn [parent n]
;;           (if (zero? n)
;;             1
;;             (* n (temp-fn parent (dec n)))))
;;        THIS-NODE n))))


;; (defmethod parse-sexp 'def
;;   [[_ name init-form] ctx]
;;   (let [ctx (merge ctx {:def-name name :recur-symb (gensym)})
;;         debugged-init (parse-item init-form ctx)
;;         debugged-init-form (:r-form debugged-init)
;;         is-recur-fn (:recur debugged-init)
;;         r-form (if nil ;;(= (first debugged-init-form) 'fn*)
;;                  (let [fn-body (second debugged-init-form)
;;                        [fn-params & body-forms] fn-body]
;;                    `(def ~name
;;                       (fn*
;;                        (~fn-params
;;                         ((fn ~(:recur-symb ctx) ~(into [(gensym)] fn-params)
;;                            ~@body-forms)
;;                          ~(:this-node ctx))))))
;;                  `(def ~name ~debugged-init-form))]
;;     {:r-form r-form
;;      :recur is-recur-fn}))

;; (defmethod parse-sexp 'fn*
;;   [form ctx]
;;   (let [[_ expr] form
;;         [params & body] expr
;;         debugged-body (map #(parse-item % ctx) body)
;;         debugged-body-forms (map #(:r-form %) debugged-body)
;;         recur (reduce #(or %1 %2) (map :recur debugged-body))]
;;     {:r-form `(fn*
;;                (~params
;;                 ~@debugged-body-forms))
;;      :recur recur}))

;; (defmethod parse-sexp :default
;;   [form ctx]
;;   (let [parent-node (:parent-node ctx)
;;         ctx (merge-node-and-update-parent ctx {})
;;         [f & params] form
;;         debugged-f (if (seq? f)
;;                      (:r-form (parse-item f ctx))
;;                      f)
;;         debugged-params (map #(parse-item % ctx) params)
;;         debugged-params-forms (map #(:r-form %) debugged-params)
;;         recur (or (reduce #(or %1 %2) (map :recur debugged-params))
;;                   (= (:def-name ctx) f))
;;         effective-f (if nil ;;(= (:def-name ctx) f)
;;                       (:recur-symb ctx)
;;                       debugged-f)
;;         effective-params (if nil ;; (= (:def-name ctx) f)
;;                            (conj debugged-params-forms (:this-node ctx))
;;                            debugged-params-forms)
;;         debugged-form `(~effective-f ~@effective-params)]
;;     {:recur recur
;;      :r-form `(let [result# ~debugged-form]
;;                 (add-transformation ~(:this-node ctx) ~parent-node (pprint result#))
;;                 result#)}))
(defmethod parse-sexp :default
  [form ctx]
  (let [[f & params] form
        debugged-f (if (seq? f)
                     (:r-form (parse-item f ctx))
                     f)
        debugged-params (map #(parse-item % ctx) params)
        debugged-params-forms (map #(:r-form %) debugged-params)
        debugged-form `(~debugged-f ~@debugged-params-forms)]
    {:r-form `(let [parent#  ~(:parent-var ctx)]
                (binding [~(:parent-var ctx) (gen-uniq-node ~(:this-node-info ctx))]
                  (let [result# ~debugged-form]
                    (add-transformation
                     ~(:parent-var ctx) ;; Contains this node info
                     parent#
                     (pprint result#))
                    result#)))}))



(defmacro xray [form]
  (let [ctx (assoc {} :parent-var (gensym))]
    `(do
       (def ~(vary-meta (:parent-var ctx) assoc :dynamic true) (gen-uniq-node {:result-node true}))
       (swap! graph (fn [a#] (lgr/weighted-digraph)))
       ~(:r-form (parse-item (mexpand-all form) ctx)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ideas for recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; If we are NOT macroexpanding at the beginnning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(xray (defn fact [n]
        (if (zero? n)
          1
          (* n (fact (dec n))))))

;; should became something like this :

(defn fact [n]
  ((fn fact2 [n2 p]
     (if (zero? n2)
       1
       (* n2 (fact2 (dec n2) p))))
   n nil))

;; If we are macroexpanding at the beginning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(fn*
   ([n] (+ 2 n)))
(def fact
  (fn*
   ([n]
      (if (zero? n)
        1
        (* n (fact (dec n)))))))

;; should became something like this :

(def fact
  (fn*
   ([n]
      ((fn temp-fn [temp-n ctx]
          (if (zero? temp-n)
            1
            (* temp-n (temp-fn (dec temp-n) ctx))))
       n nil))))


(xray (* 10
         (->> (range 5)
                 (map inc)
                 (filter #(zero? (mod % 2)))
                 (reduce +))))
