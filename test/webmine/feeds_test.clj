(ns webmine.feeds-test
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