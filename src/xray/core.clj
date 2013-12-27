(ns xray.core
  (:use clojure.tools.macro)
  (:require
   [loom.graph :as lgr]
   [loom.attr :as lat]
   [loom.alg :as lalg]
   [loom.io :as lio]
   [clojure.contrib.string :as cstr]))

(def graph (atom (lgr/digraph)))

(defn pprint [obj]
  (str (if (seq? obj) (seq obj) obj)))

(defn gen-uniq-node [node-map]
  (merge node-map {:node-id (str (gensym))}))

(defn add-transformation [v1 v2 edge]
  (swap! graph (fn [gr]
                 (-> gr
                     (lgr/add-edges [v1 v2])
                     (lat/add-attr v1 v2 :label (pprint edge))))))


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
        (let [parent#  ~(:parent-var ctx)]
          (binding [~(:parent-var ctx) (gen-uniq-node ~(:this-node-info ctx))]
            (let [result# (do ~@debugged-body-forms)]
                      (add-transformation
                       ~(:parent-var ctx) ;; Contains this node info
                       parent#
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
                    (let [parent#  ~(:parent-var ctx)]
                      (binding [~(:parent-var ctx) (gen-uniq-node ~(merge (:this-node-info ctx)
                                                                          {:bindings (zipmap (map name param-symbols)
                                                                                             param-symbols)}))]
                        (let [result# (do ~@debugged-body-forms)]
                          (add-transformation
                           ~(:parent-var ctx) ;; Contains this node info
                           parent#
                           result#)
                          result#))))}))


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
                     result#)
                    result#)))}))


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



(defmacro xray [form]
  (let [ctx (assoc {} :parent-var (gensym))]
    `(do
       (def ~(vary-meta (:parent-var ctx) assoc :dynamic true) (gen-uniq-node {:result-node true}))
       (swap! graph (fn [a#] (lgr/digraph)))
       ~(:r-form (parse-item (mexpand-all form) ctx)))))

;;;;;;;;;;;;;
;; Some tests
;;;;;;;;;;;;;

;; (xray (defn fact [n]
;;         (if (zero? n)
;;           1
;;           (* n (fact (dec n))))))

;; (xray (* 10
;;          (->> (range 5)
;;                  (map inc)
;;                  (filter #(zero? (mod % 2)))
;;                  (reduce +))))

(->> ['a
     5
     'b
     10
     'vec__7137
     '(5 9)
     'c
     '(clojure.core/nth vec__7137 0 nil)
     'd
     '(clojure.core/nth vec__7137 1 nil)]
     (partition 2)
     (remove #(cstr/substring? "vec__" (name (first %))))
     (map first))
