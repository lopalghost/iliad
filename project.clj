(defproject iliad "0.1.0-SNAPSHOT"
  :description "Iliad: A composable form library for Clojure(script)"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.9.0"]
                 [hiccup "1.0.5"]
                 [org.clojure/clojurescript "1.9.946"]
                 [hiccups "0.3.0"]]

  :source-paths ["src/clj" "src/cljc" "src/cljs"]
  :test-paths ["test/clj" "test/cljc" "test/cljs"])
