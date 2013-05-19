(defproject com.thelastcitadel/irc "0.0.1"
  :description "a rest service for irc"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.5.1"]
                 [pircbot/pircbot "1.4.2"]
                 [compojure "1.1.5"]]
  :profiles {:dev {:dependencies [[ring/ring-jetty-adapter "1.1.0"]
                                  [clj-http "0.7.2"]]}}
  :plugins [[lein-ring "0.8.5"]]
  :ring {:handler com.thelastcitadel.irc/handler})
