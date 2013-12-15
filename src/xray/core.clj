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

(defmethod parse-item :seq
  [form ctx]
  (let [f (first form)
        parent-node (or (:parent-node ctx) (gen-uniq-node {:result-node true}))
        this-node (gen-uniq-node (merge {:form (pprint form)}
                                               (when (not (= f 'quote))
                                                 {:func (pprint (first form))})))
        ctx (assoc ctx :parent-node parent-node :this-node this-node)]
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
                          ~(:parent-node ctx)
                          ~form)
      ~form)})

;;;;;;;;;;;;;;;;;;;;;;;;;;
;; S-Expressions parsing
;;;;;;;;;;;;;;;;;;;;;;;;;;


(defn merge-node-and-update-parent [ctx merge-opts]
  (let [merged (merge-with merge ctx {:this-node merge-opts})
        swapped (assoc merged :parent-node (:this-node merged))]
    swapped))

(defmethod parse-sexp 'map
  [[_ f col] ctx]
  (let [parent-node (:parent-node ctx)
        ctx (merge-node-and-update-parent ctx {:type :map
                                                :map-func (pprint f)})
        debugged-col (:r-form (parse-item col ctx))]
    {:r-form
     `(let [result# (map ~f ~debugged-col)]
        (add-transformation ~(:this-node ctx) ~parent-node (pprint result#))
        result#)}))

(defmethod parse-sexp 'reduce
  [[_ f col] ctx]
  (let [parent-node (:parent-node ctx)
        ctx (merge-node-and-update-parent ctx {:type :reduce
                                                :reduce-func (pprint f)})
        debugged-col (:r-form (parse-item col ctx))]
    {:r-form
     `(let [result# (reduce ~f ~debugged-col)]
        (add-transformation ~(:this-node ctx) ~parent-node (pprint result#))
        result#)}))



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
  (let [parent-node (:parent-node ctx)]
    {:r-form
     `(do
        (add-transformation (gen-uniq-node {})
                            ~parent-node
                            ~form)
        ~form)}))


(defmethod parse-sexp :default
  [form ctx]
  (let [parent-node (:parent-node ctx)
        ctx (merge-node-and-update-parent ctx {})
        [f & params] form
        debugged-form `(~f ~@(map #(:r-form ( parse-item % ctx)) params))]
    {:r-form
     `(let [result# ~debugged-form]
        (add-transformation ~(:this-node ctx) ~parent-node (pprint result#))
        result#)}))


(defmacro xray [form]
  `(do
     (swap! graph (fn [a#] (lgr/weighted-digraph)))
     ~(:r-form ( parse-item (mexpand-all form) {}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Ideas for recursion
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; If we are NOT macroexpanding at the beginnning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn fact [n]
  (if (zero? n)
    1
    (* n (fact (dec n)))))

;; should became something like this :

(defn fact [n]
  ((fn fact2 [n2 p]
     (if (zero? n2)
       1
       (* n2 (fact2 (dec n2) p))))
   n nil))

;; If we are macroexpanding at the beginning
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

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
