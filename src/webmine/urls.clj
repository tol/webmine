(ns webmine.urls
  (:use [webmine.core])
  (:require  [work.core :as work])
  (:import (java.net URL InetAddress
                     MalformedURLException UnknownHostException
		     HttpURLConnection Proxy)))

(defn url
  "Creates a URL. Returns nil if the url specifies and unkown protocol."
  [link] 
  (if (instance? java.net.URL link) link
  (try (java.net.URL. link)
    (catch MalformedURLException _ nil))))

(defmacro url-danger [& body]
  `(try
       ~@body
    (catch java.net.UnknownHostException _# nil)
    (catch java.io.FileNotFoundException _# nil)
    (catch java.net.UnknownServiceException _# nil)
    (catch java.lang.IllegalArgumentException _# nil)
    (catch java.io.IOException _# nil)))

(defmacro with-http-conn [[conn-sym u] & body]
  `(let [~conn-sym (cast HttpURLConnection 
			 (.openConnection (url ~u) Proxy/NO_PROXY))]
     (try
      (url-danger
       ~@body)
      (finally
       (try 
	(.close (.getInputStream ~conn-sym))
	(catch java.lang.Exception _# nil))))))

(defn url-stream
  "Creates a URL. Returns nil if the url specifies and unkown protocol."
  [link]
  (url-danger (.openStream (url link))))

;;TODO: lots of duplicaiton below: refactor
;;http://philippeadjiman.com/blog/2009/09/07/the-trick-to-write-a-fast-universal-java-url-expander/
(defn expand [u]
  ;using proxy may increase latency
  (with-http-conn [conn u]
    (doto conn
      (.setInstanceFollowRedirects false)
	       (.connect))
	(or (.getHeaderField conn "Location")
	    u)))

(defn content-type [u]
;using proxy may increase latency
  (with-http-conn [conn u]
    (doto conn
      (.setInstanceFollowRedirects false)
      (.connect))
    (.getContentType conn)))

(defn html? [u]
  (if-let [con (content-type u)]
    (.contains con "html")))

(defn path
  "Returns the path for the given URL."
   [#^URL u]
  (.getPath u))

; fetcher in nutch seems to validate hostname or ip - useful for us as well?
(defn host
  "Return the lowercased hostname for the given URL, if applicable."
   [#^URL u]
  (if-let [h (.getHost u)]
    (.toLowerCase h)))

(defn host-url [u]
 ((maybe-comp
   url
   #(str "http://" %)
   host
   url)
   u))

(defn expand-relative-urls [h xs]
  (map 
   (fn [x]
     (let [u (:url x)]
       (if (url u) x
	   (let [expanded (str (host-url h) u)]
	     (if (url expanded)
	       (assoc x :url expanded)
	       x)))))
   xs))

(defn unique-hosts
  "given a seq of links, return the unique list of expanded host urls."
  [us] 
  (seq (into #{}
	     (filter identity
		     (work/map-work (maybe-comp host-url expand) us 20)))))

(defn host-by-ip
  "Returns the IP address for the host of the given URL."
  [#^URL u]
  (try (.getHostAddress (InetAddress/getByName (host u)))
    (catch UnknownHostException _ nil)))

(defn http?
  "Returns true if the URL corresponds to the HTTP protocol."
  [#^URL url]
  (= "http" (.toLowerCase (.getProtocol url))))

; jacked from nutch: Regex pattern to get URLs within a plain text.
; http://www.truerwords.net/articles/ut/urlactivation.html
; URLs from plain text using Regular Expressions.
; http://wiki.java.net/bin/view/Javapedia/RegularExpressions
; http://regex.info/java.html @author Stephan Strittmatter - http://www.sybit.de
(def url-pattern
  #"([A-Za-z][A-Za-z0-9+.-]{1,120}:[A-Za-z0-9/](([A-Za-z0-9$_.+!*,;/?:@&~=-])|%[A-Fa-f0-9]{2}){1,333}(#([a-zA-Z0-9][a-zA-Z0-9$_.+!*,;/?:@&~=%-]{0,1000}))?)")

(defn urls
  "Takes a list of strings that could be urls, returns a list of urls,
   discarding any strings that are not valid urls."
  [us]
  (filter identity (map url us)))

(defn url-seq
  "Gets a list of urls in the text using the url pattern we ganked from nutch.
   Re-seq gives back vectors for each url, we map over the results of reseq to
   get the first member of each vector, whihc is the url."
  [t]
  (urls (map first (re-seq url-pattern t))))

(defn remove-urls
  [t]
  (.replaceAll
   (re-matcher url-pattern t)
   ""))
