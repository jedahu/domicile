(ns domicile.ns
  (:require
    [clojure.string :as str])
  (:use
    [clojure.set :only [map-invert]]))

(def xhtmlns
  "http://www.w3.org/1999/xhtml")

(def svgns
  "http://www.w3.org/2000/svg")

(def xlinkns
  "http://www.w3.org/1999/xlink")

(def xmlns
  "http://www.w3.org/XML/1998/namespace")

(def default-ns-prefixes
  {:xhtml xhtmlns
   :svg svgns
   :xlink xlinkns
   :xml xmlns})

(def ns-prefixes
  (atom {:uri default-ns-prefixes
         :prefix (map-invert default-ns-prefixes)}))

(defn add-ns-prefixes!
  [& {:as mappings}]
  (swap! ns-prefixes
         (fn [old]
           {:uri (conj (:uri old) mappings)
            :prefix (conj (:prefix old) (map-invert mappings))})))

(defn ns-uri
  [o]
  (cond
    (keyword? o) (get-in @ns-prefixes [:uri o])
    (string? o) o
    :else (throw (js/Error. "Not a namespace or ns prefix.."))))

(defn normalize-name
  [n]
  (cond
    (string? n) [nil n]
    (vector? n) [(ns-uri (first n)) (name (second n))]
    (keyword? n) (let [[uri|k k :as vect] (str/split (name n) #":")]
                     (if k
                       [(ns-uri (keyword uri|k)) k]
                       [nil uri|k]))
    (instance? js/Attr n) [(. n -namespaceURI) (. n -localName)]
    :else (throw (js/Error. "Not an attribute key."))))

(defn prefixed-name
  [n]
  (let [[ns k] (normalize-name n)]
    (if-let [prefix (get-in @ns-prefixes [:prefix ns])]
      (keyword (str (name prefix) ":" k))
      (if ns
        [ns (keyword k)]
        (keyword k)))))
