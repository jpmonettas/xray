(ns xray.handler
  (:use compojure.core
        clj-logging-config.log4j
        ring.util.response
        ring.middleware.json
        hiccup.core
        hiccup.page
        hiccup.element
        [clojure.tools.nrepl.server :only (start-server stop-server)])
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [clojure.tools.logging :as log]
            [loom.attr :as lat]
            [loom.io :as lio]
            [xray.core :as core]))

(defonce server (start-server :port 7777))

(set-loggers! "xray"
              {:level :debug
               :pattern "%d %p [%t] %c %m%n"})


(defn main-page []
  (html5
   (include-js "http://d3js.org/d3.v3.min.js"
               "http://cpettitt.github.io/project/dagre-d3/v0.0.1/dagre-d3.min.js"
               "http://cpettitt.github.io/project/graphlib-dot/latest/graphlib-dot.min.js"
               "http://code.jquery.com/jquery-2.0.3.min.js"
               "/js/xray.js")
   (include-css "/css/xray.css")
   [:div
    [:button {:id "btn-update" :type "button" } "Update graph"]]
   [:svg {:width 2000 :height 5000}
    [:g {:transform "translate(20,20)"}]]))

(defmulti htmlize-node (fn [n] (:func n)))

(defmulti htmlize-edge (fn [n1 n2] (let [lab (lat/attr @core/graph n1 n2 :label)]
                                    (cond
                                     (seq? lab) :seq
                                     (map? lab) :map
                                     (vector? lab) :vector
                                     (set? lab) :set))))

(defmethod htmlize-node 'fn*
  [n]
  (html [:div {:class "form fn"} (str (:func n))]))

(defmethod htmlize-node 'let*
  [n]
  (html [:div {:class "form let"}
         [:div {:class "special-form"} (str (:func n))]
         [:ul
          (for [[k v] (:bindings n)]
            [:li [:span k] [:span "&nbsp;->&nbsp;"] [:span v]])]]))

(defmethod htmlize-node 'if
  [n]
  (html [:div {:class "form if"}
         [:div {:class "special-form"} (str (:func n))]
         [:div {:class "if-test-form"} (str (:test-form n))]
         [:div
          [:span {:class (str "if-test-result " (if (:test-result n)
                                           "if-test-true"
                                           "if-test-false"))}
           (if (:test-result n) "true" "false")]]]))

(defmethod htmlize-node :default
  [n]
  (or (:func n) " "))

(defmethod htmlize-edge :map
  [n1 n2]
  (let [lab (lat/attr @core/graph n1 n2 :label)]
    (html [:div {:class "edge map"}
           [:div {:class "edge title"} "{&nbsp;}"]
           [:ul
            (for [[k v] lab]
              [:li [:span k] [:span "&nbsp;->&nbsp;"] [:span v]])]])))

(defmethod htmlize-edge :vector
  [n1 n2]
  (let [lab (lat/attr @core/graph n1 n2 :label)]
    (html [:div {:class "edge vector"}
           [:div {:class "edge title"} "[&nbsp;]"]
           [:ul
            (for [k lab]
              [:li [:span k]])]])))

(defmethod htmlize-edge :default
  [n1 n2]
  (let [lab (lat/attr @core/graph n1 n2 :label)]
    lab))

(defn get-graph-dot-str []
  {:body
   {:graph (lio/dot-str @core/graph :node-label htmlize-node :edge-label htmlize-edge)}})

(defroutes app-routes
  (GET "/" [] (main-page))
  (route/resources "/")
  (route/not-found "Not Found"))

(defroutes services-routes
  (GET "/graph" [] (get-graph-dot-str)))

(def app
  (handler/site (routes
                 (-> services-routes
                     (wrap-json-response))
                 app-routes)))
