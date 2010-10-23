(ns webmine.images
  (:import java.awt.Toolkit)
  (:import javax.imageio.ImageIO)
  (:require [clj-http.client :as cl])
  (:use
   infer.core
   webmine.urls
   webmine.parser))

;;http://www.mkyong.com/regular-expressions/10-java-regular-expression-examples-you-should-know/
;; (defn imgs [t]
;;   (re-seq #"([^\\s]+(\\.(?i)(jpg|png|gif|bmp))$)" t))
;;"([^\s]+(\.(?i)(jpg|png|gif|bmp))$)" t))

(defn ints [xs]
  (map #(Integer/parseInt %) xs))

(defn extract-hw
  "in case hight and width are burried inside inline style tag."
  [s]
  (re-seq #"[0-9]+" s))

(defn img? [u]
  (or (.contains u ".jpg")
      (.contains u ".png")
      (.contains u ".gif")
      (.contains u ".bmp")))

(defn img-urls [us] (filter img? us))

;;http://www.w3schools.com/tags/tag_IMG.asp
(defn size [n]
  (if-let [attrs (.getAttributes n)]
    (let [w (.getNamedItem attrs "width")
	  h (.getNamedItem attrs "height")]
      (if (and w h) (ints [(.getValue h) (.getValue w)])
	  (if-let [st (.getNamedItem attrs "style")]
	    (ints (extract-hw (.getValue st))))))))

(defn imgs [d] (map (fn [n]
		      {:url (src n)
		       :size (size n)})
		    (elements d "img")))

(defn img-area [i]
  (apply * (:size i)))

(defn big-img [images]
  ;;discard images with no size information.
  (let [is (filter :size images)]
	(max-by img-area is)))

;;example usage
;;(big-img (imgs (dom (:body (cl/get "http://gigaom.com/2010/10/22/whos-driving-mobile-payments-hint-some-are-barely-old-enough-to-drive/")))))

(defn fetch-img [u]
  (if-let [ur (url u)]
    (ImageIO/read ur)))

(defn img-size [u]
  (if-let [i (fetch-img u)]
    [(.getHeight i)
     (.getWidth i)]))

(defn fetch-sizes [imgs]
  (map 
   (fn [i]
     (if-let [s (:size i)]
       i
       (let [s (img-size (:url i))]
	 (assoc i :size s))))     
   imgs))

;;example usage
;;(big-img (fetch-sizes (imgs (dom (:body (cl/get "http://gigaom.com/2010/10/22/whos-driving-mobile-payments-hint-some-are-barely-old-enough-to-drive/"))))))

(defn extract-all [d]
  (flatten
   (map
    #(flatten (map (fn [t] (do-children t identity))
		   (do-children % identity)))
    (divs d))))

(defn big-div [d]
(max-by (comp count :textContent bean) (extract-all d)))

(defn best-img [u]
  (let [d (dom (:body (cl/get u)))
  ;;first try to get the images out of the core body div.
	core-imgs (imgs (big-div d))
	;;if that we have core images, use those, if not, get all the images in the dom
	target-imgs (if (not (empty? core-imgs))
	       core-imgs
	       (imgs d))
	eis (expand-relative-urls u target-imgs)
	;;ensure we have sizees for all images.
	sizes (fetch-sizes eis)]
    (if (empty? sizes) nil
	;;take the first image we find that has no follow image that is larger than twice it's size.
	(reduce (fn [best next]
		  (if (> (img-area next)
			 (* (img-area best) 2))
		    next
		    best))
		sizes))))

;;gets a crane:
;;http://measuringmeasures.com/blog/2010/10/11/deploying-clojure-services-with-crane.html

;;gets me:
;;http://measuringmeasures.com/blog/2010/10/21/clojure-key-value-stores-voldemort-and-s3.html

;;image with no size tags, also has later image in core body that is slightly larger, we should get the top image.
;;http://techcrunch.com/2010/10/22/stripon/

;;rolling back to all images when there are none in the body.  image is also relative path to host.
;;http://daringfireball.net/2010/10/apple_no_longer_bundling_flash_with_mac_os_x