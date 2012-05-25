(ns domicile.extended-types
  (:require
    [domicile.ns :as ns])
  (:use-macros
    [domicile.list-macro :only [extend-dom-list]]))

(extend-type js/Node
  IHash
  (-hash [o] (goog/getUid o)))

(extend-dom-list js/HTMLCollection)
(extend-dom-list js/NodeList)
(extend-dom-list js/DOMTokenList)
(extend-dom-list js/NamedNodeMap)
(extend-dom-list js/CSSStyleDeclaration)

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
      (doseq [a (. elem -attributes)]
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
    (into {} (for [k tcoll
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
