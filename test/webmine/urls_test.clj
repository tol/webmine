(ns webmine.urls-test
  (:use clojure.test
        webmine.urls)
  (:import (java.net URL MalformedURLException)))

(deftest test-url
  (is (instance? URL (url "http://google.com")))
  (is (nil? (url "/foo/bar")))
  (is (nil? (url "foo://bar"))))

(deftest test-host
  (is (= "google.com") (host (url "http://google.com/foo")))
  (is (= "google.com") (host (url "http://Google.com/foo"))))

(deftest test-path
  (are [p u] (= p (path (url u)))
    ""     "http://gooogle.com"
    "/"    "http://google.com/"
    "/foo" "http://google.com/foo"))

(deftest test-host-by-ip
  (is (nil? (host-by-ip (url "http://this-url-doesnt-exist-yet.com")))))

(deftest test-http?
  (is (http? (url "http://google.com")))
  (is (not (http? (url "https://google.com")))))

(deftest test-urls
  (is (= [(url "http://google.com") (url "http://yahoo.com")]
         (urls ["foobar" "http://google.com" "bizbat" "http://yahoo.com"]))))

(deftest test-url-seq
  (is (= [(url "http://google.com") (url "http://yahoo.com")]
         (url-seq "Yep, http://google.com is better than http://yahoo.com"))))

(deftest test-unique-hosts
  (is (= ["http://www.theamericanscholar.org" "http://www.well.com"]
	 (sort (map str 
		    (unique-hosts [(url "http://bit.ly/bkuH97")

				   (url "http://bit.ly/1Dpk5")])))))
  (is (= ["http://blog.jasonmorton.com"
          "http://blog.revolutionanalytics.com"
          "http://www.iaventurepartners.com"
          "http://www.readwriteweb.com"]
         (sort
          (map str 
               (unique-hosts
                [(url "http://www.iaventurepartners.com")
                 (url "http://blog.revolution-computing.com")
                 (url "http://blog.revolution-computing.com")
                 (url "http://www.readwriteweb.com")
                 (url "http://www.readwriteweb.com")
                 (url "http://blog.jasonmorton.com")]))))))