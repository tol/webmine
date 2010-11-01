(ns webmine.feeds-test
  (:require [clj-time.core :as time])
  (:use clojure.test
        webmine.feeds
        webmine.urls))

(deftest is-link-an-rss-feed
  (is (not (rss-suffix? "http://www.foo.com")))
  (is (rss-suffix? "http://www.foo.com/feed.xml"))
  (is (rss-suffix? "http://www.foo.com/feed.xml/"))
  (is (not (rss-suffix? "http://www.foo.com/feed.html"))))

(deftest find-rss-feeds
  (is (= ["http://www.huffingtonpost.com/feeds/original_posts/index.xml"
	  "http://feeds.huffingtonpost.com/FeaturedPosts"
	  "http://feeds.huffingtonpost.com/huffingtonpost/raw_feed"
	  "http://feeds.huffingtonpost.com/huffingtonpost/TheBlog"
	  "http://feeds.huffingtonpost.com/huffingtonpost/LatestNews"
	  "http://www.huffingtonpost.com/wires/full_index.rdf"]
	 (host-rss-feeds (url "http://www.huffingtonpost.com")))))

(deftest canonical-rss-feed
  (is (= ["http://feeds.huffingtonpost.com/FeaturedPosts"]
	 (canonical-feeds [(url "http://www.huffingtonpost.com")])))
  (is (= ["http://feeds.huffingtonpost.com/FeaturedPosts"]
	 (canonical-feeds [(url "http://www.huffingtonpost.com")
                       (url "http://www.huffingtonpost.com")]))))

(deftest date-parsing
  ;; Ensure that the datetime key strings are canonicalized to the
  ;; respective datetime value string.
  (let [datetimes {"2010-11-01T15:03:12.760Z" "2010-11-01T15:03:12.760Z"
                   "Sun, 31 Oct 2010 03:03:00 EDT" "2010-10-31T07:03:00.000Z"
                   "Sun, 31 Oct 10 03:03:00 EDT" "2010-10-31T07:03:00.000Z"
                   "Mon, 1 Nov 2010 12:24:00 PDT" "2010-11-01T19:24:00.000Z"
                   "Mon, 01 Nov 2010 12:24:00 PDT" "2010-11-01T19:24:00.000Z"
                   "1996-12-19T16:39:57-08:00" "1996-12-20T00:39:57.000Z"}]
    (doseq [[dt canonical-dt] datetimes]
      (is (= canonical-dt (@#'webmine.feeds/compact-date-time dt))))))