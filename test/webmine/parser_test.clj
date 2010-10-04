(ns webmine.parser-test
 (:use clojure.test
       webmine.parser))

(def simple-html
"
<html>
<body>

<p>foo</p>
<p>bar</p>
<p>baz</p>

</body>
</html>
")

(deftest simple-walk
  (is (= "\n\nfoo\nbar\nbaz\n\n"
    (text-from-dom (dom simple-html)))))
