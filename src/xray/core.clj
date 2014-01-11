(ns xray.core
  (:use clojure.tools.macro)
  (:require
   [loom.graph :as lgr]
   [loom.attr :as lat]
   [loom.alg :as lalg]
   [loom.io :as lio]
   [clojure.contrib.string :as cstr]))

(def graph (atom (lgr/digraph)))

(defn expand-obj [obj]
  (if (seq? obj) (seq obj) obj))

(defn gen-uniq-node [node-map]
  (merge node-map {:node-id (str (gensym))}))

(defn add-transformation [v1 v2 edge]
  (swap! graph (fn [gr]
                 (-> gr
                     (lgr/add-edges [v1 v2])
                     (lat/add-attr v1 v2 :label (expand-obj edge))))))


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

(defmethod parse-item :seq
  [form ctx]
  (let [f (first form)
        this-node-info {:func `(quote ~f)
                        :form `(quote ~form)}
        ctx (assoc ctx :this-node-info this-node-info)]
    (parse-sexp form ctx)))

;; (defmethod parse-item :map
;;   [form ctx]
;;   {:r-form
;;    (into (if (sorted? form) (sorted-map) {})
;;          (map #(:r-form (parse-item % ctx)) form))})

;; (defmethod parse-item :vector
;;   [form ctx]
;;   {:r-form
;;    (vec (map #(:r-form (parse-item % ctx)) form))})

;; (defmethod parse-item :set
;;   [form ctx]
;;   {:r-form (into (if (sorted? form) (sorted-set) #{})
;;                  (map #(:r-from (parse-item % ctx)) form))})

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


(defmethod parse-sexp 'quote
  [form ctx]
  {:r-form
   `(do
      (add-transformation (gen-uniq-node {})
                          ~(:parent-var ctx)
                          ~form)
      ~form)})



(defmethod parse-sexp 'def
  [[_ name init-form] ctx]
  (let [ctx (merge ctx {:def-name name :recur-symb (gensym)})
        debugged-init (parse-item init-form ctx)
        debugged-init-form (:r-form debugged-init)
        r-form `(def ~name ~debugged-init-form)]
    {:r-form r-form}))


(defmethod parse-sexp 'fn*
  [form ctx]
  (let [[_ expr] form
        [params & body] expr
        debugged-body (map #(parse-item % ctx) body)
        debugged-body-forms (map #(:r-form %) debugged-body)]
    {:r-form
     `(fn*
       (~params
        (let [parent-node#  ~(:parent-var ctx)
              this-node# (gen-uniq-node ~(:this-node-info ctx))]
          (binding [~(:parent-var ctx) this-node#]
            (let [result# (do ~@debugged-body-forms)]
                      (add-transformation
                       this-node#
                       parent-node#
                       result#)
                      result#)))))}))


(defmethod parse-sexp 'let*
  [[_ params & body] ctx]
  (let [param-symbols (->> params
                           (partition 2)
                           (remove #(cstr/substring? "vec__" (name (first %))))
                           (map first))
        debugged-body (map #(parse-item % ctx) body)
        debugged-body-forms (map #(:r-form %) debugged-body)]
    {:r-form `(let* ~params
                    (let [parent-node#  ~(:parent-var ctx)
                          this-node# (gen-uniq-node ~(merge (:this-node-info ctx)
                                                                          {:bindings (zipmap (map name param-symbols)
                                                                                             param-symbols)}))]
                      (binding [~(:parent-var ctx) this-node#]
                        (let [result# (do ~@debugged-body-forms)]
                          (add-transformation
                           this-node#
                           parent-node#
                           result#)
                          result#))))}))



(defmethod parse-sexp 'if
  [[_ test then else] ctx]
  (let [debugged-then-form (:r-form (parse-item then ctx))
        debugged-else-form (:r-form (parse-item else ctx))]
    {:r-form `(let [parent-node#  ~(:parent-var ctx)
                    test# ~test
                    this-node# (gen-uniq-node (merge ~(:this-node-info ctx)
                                                                   {:test-result test#
                                                                    :test-form (quote ~test)}))]
                (binding [~(:parent-var ctx) this-node#]
                  (let [ result# (if test# ~debugged-then-form ~debugged-else-form)]
                    (add-transformation
                     this-node#
                     parent-node#
                     result#)
                    result#)))}))


(defmethod parse-sexp :default
  [form ctx]
  (let [[f & params] form
        debugged-f (if (seq? f)
                     (:r-form (parse-item f ctx))
                     f)
        debugged-params (map #(parse-item % ctx) params)
        debugged-params-forms (map #(:r-form %) debugged-params)
        debugged-form `(~debugged-f ~@debugged-params-forms)]
    {:r-form `(let [parent-node#  ~(:parent-var ctx)
                    this-node# (gen-uniq-node ~(:this-node-info ctx))]
                (binding [~(:parent-var ctx) this-node#]
                  (let [result# ~debugged-form]
                    (add-transformation
                     this-node#
                     parent-node#
                     result#)
                    result#)))}))





(defmacro xray [& forms]
  (let [ctx (assoc {} :parent-var (gensym))]
    `(do
       (def ~(vary-meta (:parent-var ctx) assoc :dynamic true) (gen-uniq-node {:result-node true}))
       (swap! graph (fn [a#] (lgr/digraph)))
       ~@(map
         #(:r-form ( parse-item (mexpand-all %) ctx))
         forms))))

;;;;;;;;;;;;;
;; Some tests
;;;;;;;;;;;;;


(xray
 (defn fact [n]
   (if (zero? n)
     1
     (* n (fact (dec n)))))


 (fact
  (-
   (->> (range 5)
        (map inc)
        (reduce +))
   12)))



;; (count
;;  (zipmap
;;   [:name :age :weight]
;;   ["Peter" 30 85]))


;; (defn foo [a]
;;   (if (zero? a)
;;     1
;;     (let [j a
;;           [h k] '(6 7)]
;;       (+ a j h k (foo (dec a))))))














(defn factorial [n]
  (if (zero? n)
    1
    (* n (factorial (dec n)))))
