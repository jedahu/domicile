(ns domicile.svg
  (:require
    [domicile.core :as core]))

(defn- svg-seq
  [s]
  (when s
    (for [i (range (. s -numberOfItems))]
      (.getItem s i))))

(defrecord SvgList [list]
  ISeqable
  (-seq [this] (svg-seq list))

  ICounted
  (-count [this] (. list -numberOfItems))

  IIndexed
  (-nth
    ([this n]
     (when (< n (. list -numberOfItems))
       (. list getItem n)))
    ([list n not-found]
     (if (< n (. list -numberOfItems))
       (. list getItem n)
       not-found)))

  IReduce
  (-reduce
    ([this f]
     (ci-reduce list f))
    ([this f start]
     (ci-reduce list f start)))

  ITransientCollection
  (-conj!
    [tcoll val]
    (. list appendItem val))
  (-persistent!
    [tcoll]
    (vec (svg-seq list)))

  ITransientVector
  (-assoc-n!
    [tcoll n val]
    (. list replaceItem val n))
  (-pop! [tcoll]
    (. list removeItem (dec (. tcoll -numberOfItems)))))

(defn svg-list
  [list]
  (SvgList. list))


(deftype SvgProps [node]
  ILookup
  (-lookup
    ([o k]
     (when-let [prop (aget node (name key))]
       (cond
         (. prop -value)
         (. prop -value)

         (. prop -numberOfItems)
         (svg-list prop)

         (. prop -baseVal)
         (let [bv (. prop -baseVal)]
           (cond
             (. bv -value)         (. bv -value)
             (. bv -numberOfItems) (svg-list bv)
             :else                 bv))

         :else prop)))
    ([o k not-found]
     (or (-lookup o k) not-found)))

  ITransientAssociative
  (-assoc!
    [tcoll key val]
    (if-let [prop (aget node (name key))]
      (cond
        (. prop -value)
        (set! (. prop -value) val)

        (. prop -numberOfItems)
        (throw (js/Error. "Cannot set list property."))

        (. prop -baseVal)
        (let [bv (. prop -baseVal)]
          (cond
            (. bv -value)
            (set! (. bv -value) val)

            (. bv -numberOfItems)
            (throw (js/Error. "Cannot set list property."))

            :else (set! (. prop -baseVal) val)))

        :else (aset node (name key) val))
      (aset node (name key) val))))

(defn props
  [node]
  (SvgProps. node))
