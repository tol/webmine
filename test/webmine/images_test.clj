(ns webmine.images-test
 (:use clojure.test
       webmine.images))

(deftest extract-hw-test
  (let [s "height: 500px; width: 376px;"]
    (is (= [500 376]
	   (extract-hw s)))))