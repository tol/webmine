(ns webmine.core
  (:require [clj-http.client :as client]))

;; From l.core
(defn maybe-comp [& fs]
  (fn [x]
    (reduce
     #(if (not %1) %1 (%2 %1))
     x (reverse fs))))

;; From infer.core
(defn best-by [compare keyfn coll]
  (if (empty? coll) nil
      (let [best-finder (fn [best next-elem]
		    (if (compare (keyfn best) (keyfn next-elem))
		      best
		      next-elem))]
	(reduce best-finder coll))))

;; From infer.core
(defn max-by [keyfn coll]
  (best-by > keyfn coll))

;; From infer.core
(defn min-by [keyfn coll]
  (best-by < keyfn coll))

;; From l.fetcher
(defn body-str [u]
  (try (:body (client/get (str u)))
       (catch java.lang.Exception _ nil)))

;; From l.core
(defn min-length [us]
  (min-by (comp count str) us))