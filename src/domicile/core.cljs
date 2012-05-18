(ns domicile.core
  (:require
    [goog :as goog]))

(def xhtmlns
  "http://www.w3.org/1999/xhtml")

(def svgns
  "http://www.w3.org/2000/svg")

(def xlinkns
  "http://www.w3.org/1999/xlink")

(def xmlns
  "http://www.w3.org/XML/1998/namespace")


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


(deftype Attrs [node]
  Wrapper
  (-underlying [_] node)

  ITransientAssociative
  (-assoc!
    [tcoll key val]
    (if (vector? key)
      (. node setAttributeNS (first key) (name (second key)) val)
      (. node setAttribute (name key) val))
    tcoll)

  ITransientMap
  (-dissoc!
    [tcoll key]
    (if (vector? key)
      (. node removeAttributeNS (first key) (name (second key)))
      (. node removeAttribute (name key)))
    tcoll))

(extend-type Attrs
  ILookup
  (-lookup
    ([o k]
     (let [val (if (vector? k)
                 (. (-underlying o) getAttributeNS (first k) (name (second k)))
                 (. (-underlying o) getAttribute (name k)))]
       (if (= "" val) nil val)))
    ([o k not-found]
     (or (-lookup o k) not-found))))

(defn attrs
  [node]
  (when node (Attrs. node)))


(deftype Styles [node]
  Wrapper
  (-underlying [_] node)

  ITransientAssociative
  (-assoc!
    [tcoll key val]
    (aset (. node -style) (name key) val)
    tcoll)

  ITransientMap
  (-dissoc!
    [tcoll key]
    (aset (. node -style) (name key) "")
    tcoll))

(extend-type Styles
  ILookup
  (-lookup
    ([o k]
     (let [val (aget (. (-underlying o) -style) (name k))]
       (if (= "" val) nil val)))
    ([o k not-found]
     (or (-lookup o k) not-found))))

(defn css
  [node]
  (when node (Styles. node)))


(deftype Props [node]
  Wrapper
  (-underlying [_] node)

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
