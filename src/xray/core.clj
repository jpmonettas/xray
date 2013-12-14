(ns xray.core
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


(defmulti parse-item (fn [form ctx]
                       (cond
                        (seq? form) :seq
                        (map? form) :map
                        (vector? form) :vector
                        (set? form) :set)))

(defmethod parse-item :seq
  [form ctx]
  (parse-sexp form ctx))

(defmethod parse-item :map
  [form ctx]
  (into (if (sorted? form) (sorted-map) {})
                     (map #(parse-item %1 ctx) form)))

(defmethod parse-item :vector
  [form ctx]
  (vec (map #(parse-item %1 ctx) form)))

(defmethod parse-item :set
  [form ctx]
  (into (if (sorted? form) (sorted-set) #{})
                     (map #(parse-item %1 ctx) form)))

(defmethod parse-item :default
  [form ctx]
  (let [parent-node (:parent-node ctx)]
    `(do
       (add-transformation (gen-uniq-node {})
                           ~parent-node
                           ~form)
       ~form)))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; S-Expressions parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defmulti parse-sexp (fn [[sym & rest] ctx]
                       sym))


(defmethod parse-sexp 'map
  [[_ f col] ctx]
  (let [parent-node (:parent-node ctx)
        this-node (gen-uniq-node {:form-func "map"
                                  :map-func (pprint f)})
        ctx (assoc ctx :parent-node this-node)
        debugged-col (parse-item col ctx)]
    `(let [result# (map ~f ~debugged-col)]
       (add-transformation ~this-node ~parent-node (pprint result#))
       result#)))

(defmethod parse-sexp 'if
  [[_ test then else] ctx]
  `(if ~(parse-item test ctx)
     (do
       (print "Then")
       ~(parse-item then ctx))
     (do
       (print "Else")
       ~(parse-item else ctx))))

(defmethod parse-sexp 'quote
  [form ctx]
  (let [parent-node (:parent-node ctx)]
    `(do
       (add-transformation (gen-uniq-node {})
                           ~parent-node
                           ~form)
       ~form)))


(defmethod parse-sexp :default
  [form ctx]
  (let [parent-node (or (:parent-node ctx) (gen-uniq-node {:form-result ""}))
        this-node (gen-uniq-node {:form-func (name (first form))})
        ctx (assoc ctx :parent-node this-node)
        [f & params] form
        debugged-form `(~f ~@(map #(parse-item % ctx) params))]
    `(let [result# ~debugged-form]
       (add-transformation ~this-node ~parent-node result#)
       result#)))




(defmacro xray [form]
  `(do
     (swap! graph (fn [a#] (lgr/weighted-digraph)))
     ~(parse-item (macroexpand form) {})))
