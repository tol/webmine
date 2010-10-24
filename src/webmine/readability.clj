(ns webmine.readability
  ^{:doc "Simple heuristics for selecting content divs. Uses
    a lot from python Reability port 
    (http://github.com/srid/readability/blob/master/src/readability.py).
    
    See comment form below for example sites that work 
    and one that doesn't. "
    :author "Aria Haghighi <me@aria42.com>"}
  (:use [infer.measures :only [sparse-dot-product]]
        [clojure.contrib.def :only [defvar-]])  
  (:require [clojure.string :as clj-str]
            [webmine.parser :as parser])
  (:import [org.apache.xerces.dom ElementNSImpl]))
  
(defvar- NEGATIVE  
  #"comment|meta|footer|navigation|footnote|foot"
  "Bad words for class or ids")  

(defvar- POSITIVE  
  #"article|post|hentry|entry|content|text|body|article"
  "Good words for class or ids")  
  
(defvar- PUNC  
  #"[!\"#$%&\'()*+,-./:;<=>?@[\\]^_`{|}~]"
  "Punctuation")  
  
(defvar- SHOULD-BE-PAR
  #"<br */? *>[ \r\n]*<br */? *>"
  "Should be a paragraph break") 
  
(defn- to-binary [x]
  (if x 1.0 0.0))   
  
(defn- has-match? [s m]
  (try
    (re-seq s m)
    (catch Exception _ false)))  

; (defn is-text-div [n]
;   (let [children (parser/do-children n identity)]
;     (and children
;          (-> children count (= 1))
;          (-> children first parser/text-node?))))

(defn- div-feat-vec 
  "Features of the given div for readability"
  [#^ElementNSImpl div]
  { :num-children-pars  ; how many immediate par children    
    (count
      (filter      
        (fn [#^ElementNSImpl c] 
          (try
           (.equalsIgnoreCase (.getNodeName c) "p")
           (catch Exception _ false)))        
        (parser/do-children div identity))) 
    :only-base-children 
    (to-binary
      (every?
        (fn [#^ElementNSImpl  c]
          (or (parser/text-node? c)
              (re-matches #"a|blockquote|dl|div|img|ol|p|pre|table|ul"
                          (.getNodeName c))))
        (parser/do-children div identity)))
    :num-inner-divs
    (count
      (filter
        (fn [c]
          (and (parser/element? c) (= (.getNodeName c) "div")))
        (parser/do-children div identity)))
    :good-class-word
     (->> div parser/attr-map :class (has-match? POSITIVE) to-binary)
    :good-id-word
     (->> div parser/attr-map :id (has-match? POSITIVE) to-binary)    
    :bad-class-word 
     (->> div parser/attr-map :class (has-match? NEGATIVE) to-binary)
    :bad-id-word 
     (->> div parser/attr-map :id (has-match? NEGATIVE) to-binary)    
    :long-text?
     (if (> (-> div .getTextContent count) 10)
      1.0
      0.0)
    :num-commas
     (-> div .getTextContent frequencies (get \, 0))
   :total-words
    (->> div .getTextContent (re-seq #"\w+") count)     
    })
    ; :num-children-text-divs
    ;  (count
    ;    (filter is-text-div (parser/do-children div identity)))    
    ; :total-punc
    ;   (->> div .getTextContent (re-seq PUNC) count)    
    
        
(defvar- content-weight-vec 
  {:num-children-pars 100
   :only-base-children  10
   :num-inner-divs -1
   :long-text? 1.0
   :num-commas 1.0
   :good-class-word 25
   :good-id-word 25
   :bad-class-word -50
   :num-words 0.01
   :bad-id-word -50 }
   "Div Features set by hand from readability port")        

(defn- div-content-score [div]
  (sparse-dot-product (div-feat-vec div) content-weight-vec)) 
  
(defn find-best-content-div 
  "For the given dom root, what is the best DIV. Returns
   best DIV unaltered. May be elements in DIV that are non-content
   up to user to remove those (scripts, etc.).
   
   For best performance, consider calling strip-bad-divs
   on dom before passing in here."
  [root]
  (apply max-key div-content-score 
    (filter
       (fn [d] (>= (-> d .getTextContent count) 140))
       (parser/divs root))))
       
(defn strip-bad-divs! 
  "before finding-best-content-div use this to remove
  divs which might be bad that are likely inside the best div. 
  
  Returns modified root with bad divs removed"
  [root]
  (let [bad-divs
         (filter
            (fn [div]
              (or 
                ; Footers Bad
                (or 
                  (->> div parser/attr-map :class (has-match? #"footer"))
                  (->> div parser/attr-map :id (has-match? #"footer")))
                ; Related
                (or 
                  (->> div parser/attr-map :class (has-match? #"related")))  
                ; You can't fool me
                (let [style (-> div parser/attr-map :style)]
                  (and style
                       (re-matches #"display:\s*none;" (.toLowerCase style))))))
            (parser/divs root))]
    (parser/strip-from-dom root bad-divs)))
  

(comment 
  ;; THESE WORK
  "http://gigaom.com/2010/10/22/whos-driving-mobile-payments-hint-some-are-barely-old-enough-to-drive/"    
  "http://www.huffingtonpost.com/arianna-huffington/post_1098_b_770178.html" ; readability messes up footer too  
  "http://measuringmeasures.com/blog/2010/10/11/deploying-clojure-services-with-crane.html"
  "http://measuringmeasures.com/blog/2010/10/21/clojure-key-value-stores-voldemort-and-s3.html"
  "http://techcrunch.com/2010/10/22/stripon/" ; gets an extra bit at the end wrong, but basically good 
  "http://data-sorcery.org/2010/10/23/clojureconj/"
  "http://daringfireball.net/2010/10/apple_no_longer_bundling_flash_with_mac_os_x"
  "http://www.huffingtonpost.com/michael-moore/juan-williams-is-right-po_b_772766.html"
  ; Had to save html below locally, but work on actual HTML
  "http://www.nytimes.com/2010/10/24/world/asia/24afghan.html?_r=1&hp"
  ; parser.clj doesn't clear the scripts here
  "http://io9.com/5671733/air-force-academy-now-welcomes-spell+casters"

  ;; Mostly Work 
  ; Get content but lots of other stuff in content DIV
  "http://lifehacker.com/5671690/this-weeks-top-downloads?utm_source=feedburner&utm_medium=feed&utm_campaign=Feed:+lifehacker/full+(Lifehacker)"

  ;; DOESNT WORK - no <p> tags !
  "http://gardening.about.com/od/growingtips/tp/Tomato_Tips.htm"
 
  (-> "http://www.huffingtonpost.com/michael-moore/juan-williams-is-right-po_b_772766.html"
       java.net.URL.      
       parser/dom
       parser/strip-non-content
       strip-bad-divs!
       find-best-content-div
       .getTextContent)
       
)    