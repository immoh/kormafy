(defproject kormafy "0.1.0-SNAPSHOT"
  :description "Transform SQL to Korma DSL"
  :url "https://github.com/immoh/kormafy"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.6.0"]
                 [instaparse "1.3.5"]]
  :profiles {:dev {:dependencies [[org.clojure/test.check "0.6.1"]
                                  [korma "0.4.0"]]}})
