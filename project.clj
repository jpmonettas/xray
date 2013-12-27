(defproject org.jpmonettas/xray "0.1.0-SNAPSHOT"
  :description "Debugging macro for clojure."
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]

                 ;; For the graph
                 [aysylu/loom "0.4.2-SNAPSHOT" ] ;; Own version
                 [org.clojure/tools.macro "0.1.2"]

                 ;; For http services
                 [compojure "1.1.6"]
                 [ring/ring-json "0.2.0"]
                 [hiccup "1.0.4"]

                 ;; Utils
                 [org.clojure/tools.logging "0.2.6"]
                 [clj-logging-config "1.9.10"]
                 [org.clojure/tools.trace "0.7.6"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [org.clojure/tools.nrepl "0.2.3"]]
  :aot [loom.graph]
  :plugins [[lein-ring "0.8.8"]]
  :ring {:handler xray.handler/app}
  :profiles
  {:dev {:dependencies [[javax.servlet/servlet-api "2.5"]
                        [ring-mock "0.1.5"]]}})
