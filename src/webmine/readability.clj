(ns webmine.readability
  ^{:doc "Simple heuristics for selecting content divs. Uses
    a lot from python Reability port 
    (http://github.com/srid/readability/blob/master/src/readability.py). "
    :author "Aria Haghighi <me@aria42.com>"}
  (:use [infer.measures :only [sparse-dot-product]]
        [clojure.contrib.def :only [defvar-]])  
  (:require [clojure.string :as clj-str]
            [webmine.parser :as parser]))
  
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
  
(defn has-match? [s m]
  (try
    (re-seq s m)
    (catch Exception _ false)))  

(defn is-text-div [n]
  (let [children (parser/do-children n identity)]
    (and children
         (-> children count (= 1))
         (-> children first parser/text-node?))))

(defn- div-feat-vec [div]
  { :num-children-pars  ; how many immediate par children    
    (count
      (filter      
        (fn [c] 
          (try
           (.equalsIgnoreCase (.getNodeName c) "p")
           (catch Exception _ false)))        
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
    
        
(def content-weight-vec 
  {:num-children-pars 100
   :num-children-text-divs 50
   :long-text? 1.0
   :num-commas 1.0
   :good-class-word 25
   :good-id-word 25
   :bad-class-word -50
   :num-words 0.05
   :bad-id-word -50 })        

(defn div-content-score [div]
  (sparse-dot-product (div-feat-vec div) content-weight-vec))
  
(defn find-best-content-div [root]
  (apply max-key div-content-score (parser/divs root)))
  
(defn clean-div! [div])  

(defn find-content [root]
  (-> root find-best-content-div .getTextContent))

  
(comment  
  ; works
  "http://gigaom.com/2010/10/22/whos-driving-mobile-payments-hint-some-are-barely-old-enough-to-drive/"  
  "http://www.huffingtonpost.com/arianna-huffington/post_1098_b_770178.html"
  "http://measuringmeasures.com/blog/2010/10/11/deploying-clojure-services-with-crane.html"
  "http://measuringmeasures.com/blog/2010/10/21/clojure-key-value-stores-voldemort-and-s3.html"
  ; doesn't work
  "http://gardening.about.com/od/growingtips/tp/Tomato_Tips.htm"
  (->> "http://measuringmeasures.com/blog/2010/10/21/clojure-key-value-stores-voldemort-and-s3.html"
       java.net.URL.      
       parser/dom
       find-best-content-div
       .getTextContent)
)    