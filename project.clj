(defproject webmine "1.0.0-SNAPSHOT"
  :description "Web data mining library.

               Provides support for mining websites and newsfeeds."
  :dependencies [[org.clojure/clojure "1.2.0"]
                 [org.clojure/clojure-contrib "1.2.0"]
                 [xerces/xercesImpl "2.9.1"]
                 [org.ccil.cowan.tagsoup/tagsoup "1.2"]
                 [xalan "2.7.1"]
                 [rome "0.9"]
                 [clj-http "0.1.0-SNAPSHOT"]
                 [work "0.0.1"]]
  :dev-dependencies [[swank-clojure "1.2.1"]]
  :repositories  {"apache" "https://repository.apache.org/content/repositories/releases/"})