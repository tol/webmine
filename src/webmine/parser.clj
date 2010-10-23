(ns #^{:doc
  "parsing foo for learning machine hackers of the interwebs
  htmlparsing showdown: http://www.benmccann.com/dev-blog/java-html-parsing-library-comparison/
  revisit the HtmlParser and DOMBuilder in nutch and bixo if it seems like we
  are having issues and they ahve a few more error cases handled."}
  webmine.parser
  (:require [clojure.contrib.seq-utils :as seq])
  (:use
    clojure.xml
    webmine.core
    webmine.urls)
  (:import
    java.io.StringReader
    org.ccil.cowan.tagsoup.Parser
    (org.w3c.dom Node Document)
    (org.xml.sax XMLReader InputSource)
    (javax.xml.transform Transformer TransformerFactory)
    javax.xml.transform.sax.SAXSource
    javax.xml.transform.dom.DOMResult))

(defn dom [source]
  "html string -> dom using TagSoup.
   the features we set on the parser come from different implementations that I found in nutch, HtmlParser, as well as other parsers."
  (try
  (let [result (org.apache.xalan.xsltc.trax.SAX2DOM.)
        input (if (instance? java.net.URL source)
                (.openStream source)
                (StringReader. source))
        parser (doto (Parser.)
                 (.setContentHandler result)
                 (.setFeature Parser/namespacesFeature false)
                 (.setFeature Parser/namespacePrefixesFeature false)
                 (.setFeature Parser/bogonsEmptyFeature false)
                 (.setFeature Parser/ignoreBogonsFeature true)
                 (.parse (InputSource. input)))]
    (cast Document (.getDOM result)))
  (catch org.w3c.dom.DOMException _ )
  (catch java.io.IOException _ ))) ;;pushback buffer overflow

; const unsigned short  ELEMENT_NODE                   = 1;
; const unsigned short  ATTRIBUTE_NODE                 = 2;
; const unsigned short  TEXT_NODE                      = 3;
; const unsigned short  CDATA_SECTION_NODE             = 4;
; const unsigned short  ENTITY_REFERENCE_NODE          = 5;
; const unsigned short  ENTITY_NODE                    = 6;
; const unsigned short  PROCESSING_INSTRUCTION_NODE    = 7;
; const unsigned short  COMMENT_NODE                   = 8;
; const unsigned short  DOCUMENT_NODE                  = 9;
; const unsigned short  DOCUMENT_TYPE_NODE             = 10;
; const unsigned short  DOCUMENT_FRAGMENT_NODE         = 11;
; const unsigned short  NOTATION_NODE                  = 12;

(defn element? [node]
 (and node (= (.getNodeType node) Node/ELEMENT_NODE)))

(defn text-node? [node]
  (and node (= (.getNodeType node) Node/TEXT_NODE)))

(defn script-node? [node]
  (and node (= (.getNodeName node) "script")))
        
(defn attr [n a]
  (if-let [attrs (.getAttributes n)]
    (if-let [att (.getNamedItem attrs a)]
      (.getValue att))))
        
        
(defn attr-map 
  "returns node attributes as map of keyword attribute keys to str value"
  [n]
  (when-let [attrs (.getAttributes n)]
       (into {}
          (for [i (range (.getLength attrs))
                :let [item (bean (.item attrs i))]]
            [(-> item :name keyword) (:textContent item)]))))      

(defn href [n] (attr n "href"))
(defn src [n] (attr n "src"))
(defn node-type [n] (attr n "type"))


;;TODO: script thing still not working?
(defn extract-text [n]
  (if (or
       (not (text-node? n))
       (script-node? n))
    ""
    (.getNodeValue n)))

(defn extract-href [n]
  (if-let [link (href n)]
    link ""))

(defn elements
  "gets the elements of a certian name in the dom
   (count (divs (dom (:body (fetch (url \"http://ftalphaville.ft.com/\")))))) -> 199"
  [d #^String t]
  (let [shitty-data-structure (.getElementsByTagName d t)]
    (for [i (range 0 (.getLength shitty-data-structure))]
      (.item shitty-data-structure i))))

(defn divs
  "gets the divs in a dom.
   (count (divs (dom (:body (fetch (url \"http://ftalphaville.ft.com/\")))))) -> 199"
  [d]
  (elements d "div"))

(defn anchors [d] (elements d "a"))

(defn head [d]
  (.item
   (.getElementsByTagName d "head")
   0))

(defn hrefs [es]
  (filter (comp not nil?) (map (comp url href) es)))

(defn do-children [n f]
  (if (not (.hasChildNodes n))
    []
    (let [children (.getChildNodes n)]
      (doall (for [i (range 0 (.getLength children))]
	       (f (.item children i)))))))

(defn walk-dom
  "recursively walk the dom.
  combine: result of visiting a single node & rest -> combines them as
  appropriate for the representational structure
  init: initilial value for combine
  visit: visits one node and produces a result"
  [d visit combine]
  (let [extractor (fn extract [n]
                    (combine (visit n)
			     (do-children n extract)))]
    (extractor d)))

(defn text-from-dom
  "recursively get the text content from Nodes.
   inspired by: http://www.prasannatech.net/2009/02/convert-html-text-parser-java-api.html"
  [d]
  (walk-dom
    d
    extract-text
    (fn [head tail]
      (let [results (cons head (flatten tail))
            buffer  (StringBuffer.)]
        (doall (map #(.append buffer %) results))
        (str buffer)))))

;;(hrefs (elements (head d) "link"))
;;(links-from-dom (head d))
(defn links-from-dom
  "recursively get the links content from Nodes."
  [d]
  (flatten (walk-dom
    d
    extract-href
    (fn [head tail]
      (filter (comp not empty?) (cons head tail))))))

(defn count-with
  "count how many nodes match a pred.
  usage:  (count-with some-dom element?) -> number of nodes of type element-node"
  [d pred]
  (walk-dom
    d
    (fn [n] (if (pred n) 1 0))
    (fn [head tail]
      (apply + (flatten (cons head tail))))))

(defn extract-features [response]
  (assoc response
    :body ((maybe-comp text-from-dom dom :body) response)))
