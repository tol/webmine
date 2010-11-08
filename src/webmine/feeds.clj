(ns webmine.feeds
  (:use clojure.xml
        clojure.set
        clojure.contrib.java-utils
        webmine.core
        webmine.parser
        webmine.urls
        [clojure.java.io :only [input-stream]])
  (:require [work.core :as work]
            [clojure.zip :as zip]
            [clojure.contrib.logging :as log]
            [clojure.contrib.zip-filter :as zip-filter]
            [clojure.contrib.zip-filter.xml :as xml-zip]
            [clj-time.format :as time-fmt]
            [clj-time.coerce :as time-coerce])  
  (:import [com.sun.syndication.feed.synd
            SyndFeedImpl SyndEntryImpl SyndContentImpl]
           [com.sun.syndication.io
            SyndFeedInput SyndFeedOutput XmlReader]
           java.util.Date
           java.util.ArrayList
           java.io.InputStream
           [java.text
            SimpleDateFormat ParsePosition]))

(def rfc822-rss-formats
     [(SimpleDateFormat. "E, dd MMM yy HH:mm:ss Z")
      (SimpleDateFormat. "E, dd MMM yyyy HH:mm:ss Z")])

(defn- compact-date-time [s]
  (let [date-time (first
                   (filter identity
                           (concat (map (fn [f]
                                          (try
                                            (time-fmt/parse f s)
                                            (catch Exception _ nil)))
                                        (vals time-fmt/formatters))
                                   (map (fn [sdf]
                                          (try (when-let [d (.parse sdf s (ParsePosition. 0))]
                                                 (time-coerce/from-date d))
                                               (catch Exception _ nil)))
                                        rfc822-rss-formats))))]
    (time-fmt/unparse (time-fmt/formatters :date-time) date-time)))

(defn mk-des [entry]
  (if (and (:des entry)
	   (not (= (:des entry)
		   (:content entry))))
    entry
;;TODO: keep formatting but not img tags for web client?
    (let [d (text-from-dom
	     (first (elements (dom (:content entry)) "p")))]
      (assoc entry :des d))))

(defn- item-node-to-entry [item]
  (let [item-root (zip/xml-zip item)
	get-text (fn [k] (xml-zip/xml1-> item-root k xml-zip/text))
	entry
	{:title (get-text :title)
	 :link (get-text :link)
	 :content (apply max-key count
			 (map get-text [:content :description :content:encoded]))
	 :des (first (filter identity
			     (map get-text [:description :content :content:encoded])))
	 :date (try (first (for [k [:pubDate :date :updatedDate]
				 :let [s (get-text k)]
				 :when k] (if s (compact-date-time s)
					      nil)))
		    (catch Exception e (log/error e)))
	 :author (get-text :author)}]
    (try (mk-des entry)
         (catch Exception _
           entry))))

(defn- str-to-url [s]
  (if (string? s) (java.net.URL. s) s))

(defn parse-feed [source]
  "returns map representing feed. Supports keys
  :title Name of feed
  :des Description of feed
  :link link to feed
  :entries seq of entries, see doc below for entries"
  (try
    (when-let [root (-> source str-to-url input-stream parse zip/xml-zip)]
     {:title (xml-zip/xml1-> root :channel :title xml-zip/text)
      :des   (xml-zip/xml1-> root :channel :description xml-zip/text)
      :link  (xml-zip/xml1-> root :channel :link xml-zip/text)
      :entries
      (doall
       (for [n (xml-zip/xml-> root 
			      :channel :item zip/node)
	     :let [entry (into {}
			       (filter second
				       (item-node-to-entry n)))]]
	 entry))})
    (catch Exception e (.printStackTrace e) nil)))

(defn entries
  "return seq of entries from rss feed source (must be File or URL).
  Each entry is a map with string values
  :title entry title
  :des  descritpion
  :date String of date
  :author author string
  :content Content of entry (or :description if not content)
  :link Link to content. "
  [source]
  (-> source parse-feed  :entries))

(defn feed? [item]
  (and item
       (let [feed (parse-feed item)]
	 (:entries feed))))

(defn links-from-entry [e]
 (-> e :content url-seq))

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
    (:link synd-feed)))

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
  (let [u (str u)]
    (or (.contains u "xml")
	(.contains u "rss")
	(.contains u "atom")
	#_(.matches u "^.*/[^/.]*$"))))
;; (let [su (str u)
;; 	l (.length su)
;; 	drop (= "/" (.charAt su (- l 1)))
;; 	u* (if (not drop) su (subs su 0 (- l 1)))]
;; 	(not (or (.endsWith u* ".com")
;; 	    (.endsWith u* ".html")
;; 	    (.endsWith u* ".php")
;; 	    (.endsWith u* ".png")
;; 	    (.endsWith u* ".ico")
;; 	    (.endsWith u* ".txt")
;; 	    (.endsWith u* ".txt"))))

(defn- fix-link
  "fix absolute links"
  [base #^String link]
  (if (and link (.startsWith link "/"))
    (str base link)
    link))

(defn host-rss-feeds
  "checks head of page for rss feed links. Does two checks, any links
   that are marked as rss/xml/atom in the link type or if
   any link has rss xml or  "
  [page]
  (let [d (-> page body-str dom)]    
    (into #{}	  
    (filter identity #_(comp feed? url)
	    (concat
	     ;; sometimes rss feeds are marked as
	     ;; <link type="application/rss+xml">
	     (when-let [all-links (elements d "link")]
	       (for [l all-links
		     :when l
		     :let [attr (attr-map l)
			   #^String type (:type attr)
			   #^String link (->> attr :href (fix-link (str page)))]
		     :when (and link
			        type
				(or (.contains type "rss") (.contains type "atom"))) ]
		 link))
	     ;;most sites go with the standard that the rss or atom feed is in the head
	     (when-let [head-links (-> d head links-from-dom)]
	       (->> head-links
		    (map (partial fix-link (str page)))
		    (filter #(and (not (comment? %)) (rss-suffix? %))))))))))

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

(defn find-feed-outlinks
  "given the url of a blog's homepage, find the outlinks to feeds from the homepage."
  [b u]
  (let [outs (into #{}
		   (find-outlinks b u))
	feeds (filter
	       identity
	       (canonical-feeds outs))
	;;we need to filter same host feeds again, as they can get filtered from outlinsk but then be found again when extracting canonical feeds.
	ex-feeds (into #{} (filter #(external? (url u) (url %))
				   feeds))]
    (seq ex-feeds)))

(defn home-feed-outlinks
[u]
  (find-feed-outlinks (body-str u) u))

(defn entry-feed-outlinks
  "given the url of a blog's feed, find the outlinks to feeds from all the entries currently in this blog's feed."
  [u]
  (let [home (feed-home (url u))
	uber-b (apply str (entries (url u)))]
    (find-feed-outlinks uber-b u)))

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


(comment
  (entries "http://www.rollingstone.com/siteServices/rss/allNews")
  (entries (java.net.URL. "http://www.rollingstone.com/siteServices/rss/allNews"))
  (canonical-feed "http://www.rollingstone.com/")
  (canonical-feed "http://techcrunch.com/2010/11/02/andreessen-horowitz-650m-fund/")
  ; This one requires fix-link, otherwise doesn't work
  (canonical-feed "http://npr.org")
  (canonical-feed "http://io9.com/")
  (canonical-feed "http://www.huffingtonpost.com/")
)


