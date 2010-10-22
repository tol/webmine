(ns webmine.images
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

(defn big-img [images]
  ;;discard images with no size information.
  (let [is (filter :size images)]
	(max-by #(apply * (:size %)) is)))

;;(big-img (imgs (dom (:body (cl/get "http://gigaom.com/2010/10/22/whos-driving-mobile-payments-hint-some-are-barely-old-enough-to-drive/")))))