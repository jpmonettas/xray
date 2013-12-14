(defproject org.jpmonettas/xray "0.1.0-SNAPSHOT"
  :description "Debugging macro for clojure."
  :url ""
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [aysylu/loom "0.4.1" ]
                 [org.clojure/tools.macro "0.1.2"]]
  :aot [loom.graph])
