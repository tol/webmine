(defproject webmine "0.1.1-SNAPSHOT"
  :description "Web data mining library.

               Provides support for mining websites and newsfeeds."
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [xerces/xercesImpl "2.9.1"]
                 [org.ccil.cowan.tagsoup/tagsoup "1.2"]
                 [xalan "2.7.1"]
                 [rome "0.9"]
                 [infer "1.0-SNAPSHOT"]
                 [clj-http "0.1.0-SNAPSHOT"]
                 [work "0.0.1-SNAPSHOT"]
                 [clj-time "0.1.0-RC1"]]
  :dev-dependencies [[swank-clojure "1.3.0-SNAPSHOT"]
                     [lein-clojars "0.5.0"]]
  :repositories  {"apache" "https://repository.apache.org/content/repositories/releases/"})