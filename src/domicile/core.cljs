(ns domicile.core
  (:require
    [domicile.ns :as ns]
    [clojure.string :as str]
    [goog :as goog])
  (:use
    [clojure.set :only [map-invert]]))

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


(extend-type js/Element
  ITransientCollection
  (-conj!
    [elem o]
    (if (satisfies? IMapEntry o)
      (-assoc! elem (key o) (val o))
      (do
        (doseq [e o]
          (-assoc! elem (key e) (val e)))
        elem)))
  (-persistent!
    [elem]
    (let [map (transient {})]
      (doseq [a (dom-list (. elem -attributes))]
        (assoc! map
                (ns/prefixed-name a)
                (. a -value)))
      (persistent! map)))

  ITransientAssociative
  (-assoc!
    [elem key val]
    (let [[ns k] (ns/normalize-name key)]
      (. elem setAttributeNS ns k val))
    elem)

  ITransientMap
  (-dissoc!
    [elem key]
    (let [[ns k] (ns/normalize-name key)]
      (. elem removeAttributeNS ns k))
    elem)

  ILookup
  (-lookup
    ([elem k]
     (let [[ns k] (ns/normalize-name k)
           val (. elem getAttributeNS ns k)]
       (if (= "" val) nil val)))
    ([elem k not-found]
     (or (-lookup elem k) not-found))))


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
    (into {} (for [k (dom-list tcoll)
                   :let [v (. tcoll getPropertyValue k)]
                   :when (and v (not= "" v))]
               [(keyword k) v])))

  ITransientAssociative
  (-assoc!
    [tcoll key val]
    (. tcoll setProperty (name key) val nil)
    tcoll)

  ITransientMap
  (-dissoc!
    [tcoll key]
    (. tcoll removeProperty (name key))
    tcoll)

  ILookup
  (-lookup
    ([o k]
     (let [val (. o getPropertyValue (name k))]
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
