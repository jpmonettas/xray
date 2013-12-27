(ns xray.handler
  (:use compojure.core
        clj-logging-config.log4j
        ring.util.response
        ring.middleware.json
        hiccup.core
        hiccup.page
        [clojure.tools.nrepl.server :only (start-server stop-server)])
  (:require [compojure.handler :as handler]
            [compojure.route :as route]
            [clojure.tools.logging :as log]
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

(defmethod htmlize-node 'fn*
  [n]
  (html [:div {:class "form fn"} (str (:form n))]))

(defmethod htmlize-node :default
  [n]
  (or (:form n) " "))

(defn get-graph-dot-str []
  {:body
   {:graph (lio/dot-str @core/graph :node-label htmlize-node)}})

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
