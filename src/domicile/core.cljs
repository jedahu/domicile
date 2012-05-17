(ns domicile.core
  (:require
    [goog :as goog]))

(extend-type js/Node
  IHash
  (-hash [o] (goog/getUid o)))

(defprotocol NodeWrapper
  (-dom-node [wrapper]))

(deftype Attrs [node]
  NodeWrapper
  (-dom-node [_] node)

  ITransientAssociative
  (-assoc!
    [tcoll key val]
    (if (vector? key)
      (. node setAttributeNS (first key) (name (second key)) val)
      (. node setAttribute (name key) val)))

  ITransientMap
  (-dissoc!
    [tcoll key]
    (if (vector? key)
      (. node removeAttributeNS (first key) (name (second key)))
      (. node removeAttribute (name key)))))

(extend-type Attrs
  ILookup
  (-lookup
    ([o k]
     (if (vector? k)
       (. (-dom-node o) getAttributeNS (first k) (name (second k)))
       (. (-dom-node o) getAttribute (name k))))
    ([o k not-found]
     (or (-lookup o k) not-found))))

(defn attrs
  [node]
  (Attrs. node))


(deftype Styles [node]
  NodeWrapper
  (-dom-node [_] node)

  ITransientAssociative
  (-assoc!
    [tcoll key val]
    (aset (. node -style) (name key) val))

  ITransientMap
  (-dissoc!
    [tcoll key]
    (aset (. node -style) (name key) "")))

(extend-type Styles
  ILookup
  (-lookup
    ([o k]
     (let [val (aget (. (-dom-node o) -style) (name k))]
       (if (= "" val) nil val)))
    ([o k not-found]
     (or (-lookup o k) not-found))))

(defn css
  [node]
  (Styles. node))


(deftype Props [node]
  NodeWrapper
  (-dom-node [_] node)

  ITransientAssociative
  (-assoc!
    [tcoll key val]
    (aset node (name key) val))

  ITransientMap
  (-dissoc!
    [tcoll key]
    (aset node (name key) nil)))

(extend-type Props
  ILookup
  (-lookup
    ([o k]
     (aget (-dom-node o) (:name k)))
    ([o k not-found]
     (or (-lookup o k) not-found))))

(defn props
  [node]
  (Props. node))
