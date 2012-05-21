(ns domicile.core
  (:require
    [clojure.string :as str]
    [goog :as goog])
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

(defn normalize-attr-key
  [key]
  (cond
    (string? key) [nil key]
    (vector? key) [(ns-uri (first key)) (name (second key))]
    (keyword? key) (let [[uri|k k :as vect] (str/split (name key) #":")]
                     (if k
                       [(ns-uri (keyword uri|k)) k]
                       [nil uri|k]))
    (instance? js/Attr key) [(. key -namespaceURI) (. key -localName)]
    :else (throw (js/Error. "Not an attribute key."))))

(defn prefix-attr-key
  [key]
  (let [[ns k] (normalize-attr-key key)]
    (if-let [prefix (get-in @ns-prefixes [:prefix ns])]
      (keyword (str (name prefix) ":" k))
      (if ns
        [ns (keyword k)]
        (keyword k)))))


(extend-type js/Node
  IHash
  (-hash [o] (goog/getUid o)))


(defprotocol Wrapper
  (-underlying [wrapper]))

(defn underlying
  [wrapper]
  (-underlying wrapper))

(deftype DomList [list]
  Wrapper
  (-underlying [_] list)

  ISeqable
  (-seq
    [this]
    (for [i (range 0 (. list -length))]
      (. list item i)))

  ICounted
  (-count
    [this]
    (. list -length)))

(extend-type DomList
  IReduce
  (-reduce
    ([this f]
     (ci-reduce this f))
    ([this f start]
     (ci-reduce this f start)))

  IIndexed
  (-nth
    ([this n]
     (when (< n (. (-underlying this) -length))
       (. (-underlying this) item n)))
    ([this n not-found]
     (if (< n (. (-underlying this) -length))
       (. (-underlying this) item n)
       not-found))))

(defn dom-list
  [list]
  (when list (DomList. list)))


(deftype Attrs [elem]
  Wrapper
  (-underlying [_] elem)

  ITransientCollection
  (-conj!
    [tcoll o]
    (if (satisfies? IMapEntry o)
      (-assoc! tcoll (key o) (val o))
      (do
        (doseq [e o]
          (-assoc! tcoll (key e) (val e)))
        tcoll)))
  (-persistent!
    [tcoll]
    (let [map (transient {})]
      (doseq [a (dom-list (. elem -attributes))]
        (assoc! map
                (prefix-attr-key a)
                (. a -value)))
      (persistent! map)))

  ITransientAssociative
  (-assoc!
    [tcoll key val]
    (let [[ns k] (normalize-attr-key key)]
      (. elem setAttributeNS ns k val))
    tcoll)

  ITransientMap
  (-dissoc!
    [tcoll key]
    (let [[ns k] (normalize-attr-key key)]
      (. elem removeAttributeNS ns k))
    tcoll))

(extend-type Attrs
  ILookup
  (-lookup
    ([o k]
     (let [[ns k] (normalize-attr-key k)
           val (. (-underlying o) getAttributeNS ns k)]
       (if (= "" val) nil val)))
    ([o k not-found]
     (or (-lookup o k) not-found))))

(defn attrs
  [elem]
  (when elem (Attrs. elem)))

(extend-type js/CSSStyleDeclaration
  ITransientCollection
  (-conj!
    [tcoll o]
    (if (satisfies? IMapEntry o)
      (-assoc! tcoll (key o) (val o))
      (do
        (doseq [e o]
          (-assoc! tcoll (key e) (val e)))
        tcoll)))
  (-persistent!
    [tcoll]
    (into {} (for [k (js-keys tcoll)
                   :let [v (aget tcoll k)]
                   :when (and v (not= "" v))]
               [(keyword k) v])))

  ITransientAssociative
  (-assoc!
    [tcoll key val]
    (aset tcoll (name key) val)
    tcoll)

  ITransientMap
  (-dissoc!
    [tcoll key]
    (aset tcoll (name key) "")
    tcoll)

  ILookup
  (-lookup
    ([o k]
     (let [val (aget o (name k))]
       (if (= "" val) nil val)))
    ([o k not-found]
     (or (-lookup o k) not-found))))


(deftype Props [node]
  Wrapper
  (-underlying [_] node)

  ITransientCollection
  (-conj!
    [tcoll o]
    (if (satisfies? IMapEntry o)
      (-assoc! tcoll (key o) (val o))
      (do
        (doseq [e o]
          (-assoc! tcoll (key e) (val e)))
        tcoll)))
  (-persistent!
    [tcoll]
    (throw (js/Error. "Deliberately not implemented.")))

  ITransientAssociative
  (-assoc!
    [tcoll key val]
    (aset node (name key) val)
    tcoll)

  ITransientMap
  (-dissoc!
    [tcoll key]
    (aset node (name key) nil)
    tcoll))

(extend-type Props
  ILookup
  (-lookup
    ([o k]
     (let [val (aget (-underlying o) (name k))]
       (if (and val (. val -item))
         (dom-list val)
         val)))
    ([o k not-found]
     (or (-lookup o k) not-found))))

(defn props
  [node]
  (when node (Props. node)))
