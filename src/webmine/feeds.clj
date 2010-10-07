(ns webmine.feeds
  (:use clojure.xml
        clojure.set
        clojure.contrib.java-utils
        webmine.core
        webmine.parser
        webmine.urls)
  (:require [work.core :as work])
  (:import [com.sun.syndication.feed.synd
            SyndFeedImpl SyndEntryImpl SyndContentImpl]
           [com.sun.syndication.io
            SyndFeedInput SyndFeedOutput XmlReader]
           java.util.Date
           java.util.ArrayList
           java.io.InputStream))

(defn parse-feed
  "takes an InputSream, Url, or File." 
  [source]
  (try 
  (.build (SyndFeedInput.) (XmlReader. source))
   (catch com.sun.syndication.io.ParsingFeedException _ _
	  nil)
   (catch java.io.IOException _ _
	  nil)))

(defn feed? [source]
      (and source (parse-feed source)))

(defn entries [source]
  (if-let [synd-feed (parse-feed source)]
    (seq (.getEntries synd-feed))))

(defn body [e]
  (let [d (.getDescription e)
	c (first (seq (.getContents e)))]
    (cond (not (or d c)) ""
	  (not d) (.getValue c)
	  (not c) (.getValue d)
	  :else (max-by count
			[(.getValue d) (.getValue c)]))))

(defn entry-as-map [e]
   (into {} (map #(if (nil? (second %)) [(first %) ""] %)
   {:date (str (.getPublishedDate e))
    :author (.getAuthor e)
    :title (.getTitle e)
    :link (.getLink e)
    :des (let [d (body e)]
	   (if (empty? d) d
	       (text-from-dom (dom d))))})))

(defn links-from-entry [e]
 (url-seq (body e)))

(defn text-content [v]
(doto (SyndContentImpl.)
		       (.setType com.sun.syndication.feed.atom.Content/TEXT)
		       (.setValue v)))

(defn map-to-entry [entry]
  (let [content-keys (difference
		      (into #{} (keys entry))
		      #{:title :link :author :date :des})
	content (map (fn [k]
		       (let [v (entry k)]
			 [k (.setValue (SyndContentImpl.) v)]))
		     content-keys)]

  (doto (SyndEntryImpl.)
    (.setTitle (:title entry))
    (.setLink (:link entry))
    (.setAuthor (:author entry))
    (.setPublishedDate (:date entry))
    (.setDescription
     (doto (SyndContentImpl.)
       (.setType "text/html")
       (.setValue  (:des entry)))))))

(defn feed-home [source]
 (if-let [synd-feed (parse-feed source)]
    (.getLink synd-feed)))

(defn external?
  "is the url from the same host as the home url?"
  [home other]
  (and home other
       (not (= (.getHost other)
	       (.getHost home)))))

(def internal? (complement external?))

(defn external
  "removes urls to the same host"
  [home urls]
  (filter #(external? (url home) %)
	  urls))

(defn external-feed? [home other]
  (and (external? home other)
       (feed? other)))

(defn internal-feed? [home other]
  (and (internal? home other)
       (feed? other)))

;;TODO: refactor to a sinlge api - url, string url, etc.
(defn find-outlinks
"string (page body) -> url (blog homepage) -> outlinks"
[s h]
  (seq (into #{}
	     (work/filter-work
	      #(external? (url h) %)
	      (url-seq s)
	      20))))

(defn comment? [u]
(.contains (str u) "comments"))

;;TODO: invert to make mroe efficient with and.
;;also could invert conditional to be only .xml, /, or nothing
(defn rss-suffix? [u]
  (let [su (str u)
	l (.length su)
	drop (= "/" (.charAt su (- l 1)))
	u* (if (not drop) su (subs su 0 (- l 1)))]
	(not (or (.endsWith u* ".com")
	    (.endsWith u* ".html")
	    (.endsWith u* ".php")
	    (.endsWith u* ".png")
	    (.endsWith u* ".ico")
	    (.endsWith u* ".txt")
	    (.endsWith u* ".txt")))))

(defn host-rss-feeds [page]
  ;;most sites go with the standard that the rss or atom feed is in the head
  (if-let [head-links ((maybe-comp links-from-dom head dom body-str) page)]
    (seq (into #{}
	       (filter #(and (not (comment? %))
			     (rss-suffix? %)
			     (feed? (url %)))
		       head-links)))))

(def canonical-feed (comp min-length host-rss-feeds))

(defn canonical-feeds
"
Avoid subscribing to multiple feeds on the same blog.
Initial heuristic is to take url with min length.
May not be a good idea for blogs that have many useful feeds, for example, for a news site like huffington post."
[urls]
(seq (into #{} (work/map-work canonical-feed urls 20))))

;;TODO: parallelize
(defn blogroll [opml]
  (for [x (xml-seq (parse opml))
        :when (= :outline (:tag x))
	:let [a (:attrs x)
	      u (or (:xmlUrl a) (:htmlUrl a))]
	:when (url u)]
    u))

(defn home-feed-outlinks
  "given the url of a blog's homepage, find the outlinks to feeds from the homepage."
[u]
  (let [outs (into #{}
		   (find-outlinks (body-str u) u))
	feeds (filter
	       identity
	       (canonical-feeds outs))
	;;we need to filter same host feeds again, as they can get filtered from outlinsk but then be found again when extracting canonical feeds.
	ex-feeds (into #{} (filter #(external? (url u) (url %))
				   feeds))]
    (seq ex-feeds)))

;;;map of feed home -> rss url
;;    (zipmap (map #(feed-home (url %)) ex-feeds) ex-feeds)))

(defn entry-feed-outlinks
  "given the url of a blog's feed, find the outlinks to feeds from all the entries currently in this blog's feed."
[u]
  (let [home (feed-home (url u))
	outs (into #{}
		   (flatten
		    (map
		     #(find-outlinks (body %) home)
		     (entries (url u)))))
	feeds (filter
	       identity
	       (canonical-feeds outs))
	;;we need to filter same host feeds again, as they can get filtered from outlinsk but then be found again when extracting canonical feeds.
	ex-feeds (into #{} (filter #(external? (url u) (url %))
				   feeds))]
    (seq ex-feeds)))

(defn feed-outlinks
  "given the url of a blog's homepage or rss feed, find the outlinks to feeds from both the homepage, and all the entries currently in this blog's feed."
  [u]
  (let [h (feed-home (url u))
	[home fd] (if h [h u]
		      [u (canonical-feed u)])]
    {:homepage [home (home-feed-outlinks home)]
     :entries [fd (entry-feed-outlinks fd)]}))

(defn merge-outlinks [outlinks-map]
  (into #{} (concat (second (:entries outlinks-map))
		    (second (:homepage outlinks-map)))))