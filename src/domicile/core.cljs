(ns domicile.core
  (:require
    [goog :as goog]))

(extend-type js/Node
  IHash
  (-hash [o] (goog/getUid o)))

(defn ns-key
  [k]
  (and (vector? k) k))

(deftype Attrs [node]
  ILookup
  (-lookup
    ([o k]
     (if-let [[ns k] (ns-key k)]
       (. node getAttributeNS ns (name k))
       (. node getAttribute (name k))))
    ([o k not-found]
     (or (-lookup o k) not-found)))

  ITransientAssociative
  (-assoc!
    [tcoll key val]
    (if-let [[ns key] (ns-key key)]
      (. node setAttributeNS ns (name key) val)
      (. node setAttribute (name key) val)))

  ITransientMap
  (-dissoc!
    [tcoll key]
    (if-let [[ns key] (ns-key key)]
      (. node removeAttributeNS ns (name key))
      (. node removeAttribute (name key)))))

(defn attrs
  [node]
  (Attrs. node))


(deftype Styles [node]
  ILookup
  (-lookup
    ([o k]
     (aget (. node -style) (name key)))
    ([o k not-found]
     (or (-lookup o k) not-found)))

  ITransientAssociative
  (-assoc!
    [tcoll key val]
    (aset (. node -style) (name key) val))

  ITransientMap
  (-dissoc!
    [tcoll key]
    (aset (. node -style) (name key) "")))

(defn css
  [node]
  (Styles. node))


(deftype Props [node]
  ILookup
  (-lookup
    ([o k]
     (aget node (:name k)))
    ([o k not-found]
     (or (-lookup o k) not-found)))

  ITransientAssociative
  (-assoc!
    [tcoll key val]
    (aset node (name key) val))

  ITransientMap
  (-dissoc!
    [tcoll key]
    (aset node (name key) nil)))

(defn props
  [node]
  (Props. node))
