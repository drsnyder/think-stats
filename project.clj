(defproject think-stats "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :repl-options { :init-ns think-stats.repl-helper }
  :profiles {:dev {:plugins [[lein-midje "3.0.0"]]
                   :dependencies []}}
  :dependencies [[org.clojure/clojure "1.4.0"]
                 [lonocloud/synthread "1.0.3"]
                 [org.clojure/data.csv "0.1.2"]
                 [midje "1.5.0"]
                 [clj-http "0.7.2"]
                 [environ "0.2.1"]]
  :jvm-opts ["-Xmx512m" "-server"])
