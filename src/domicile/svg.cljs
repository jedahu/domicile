(ns domicile.svg
  (:require
    [domicile.core :as dom]))

(def svgns
  "http://www.w3.org/2000/svg")

(deftype SvgList [list]
  dom/Wrapper
  (-underlying [_] list)

  ISeqable
  (-seq
    [this]
    (for [i (range 0 (. list -numberOfItems))]
      (. list getItem i)))

  ICounted
  (-count
    [this]
    (. list -numberOfItems))

  ITransientAssociative
  (-assoc!
    [tcoll key val]
    (-assoc-n! tcoll key val))

  ITransientCollection
  (-conj!
    [tcoll val]
    (. list appendItem val)
    tcoll)
  (-persistent!
    [tcoll]
    (vec (-seq tcoll)))

  ITransientVector
  (-assoc-n!
    [tcoll n val]
    (. list replaceItem n val)
    tcoll)
  (-pop!
    [tcoll]
    (. list removeItem (dec (. tcoll -numberOfItems)))
    tcoll))

(extend-type SvgList
  IIndexed
  (-nth
    ([this n]
     (when (< n (. (dom/-underlying this) -numberOfItems))
       (. (dom/-underlying this) getItem n)))
    ([this n not-found]
     (if (< n (. (dom/-underlying this) -numberOfItems))
       (. (dom/-underlying this) getItem n)
       not-found)))

  IReduce
  (-reduce
    ([this f]
     (ci-reduce this f))
    ([this f start]
     (ci-reduce this f start))))

(defn svg-list
  [list]
  (when list (SvgList. list)))


(deftype SvgProps [node]
  dom/Wrapper
  (-underlying [_] node)

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
      (aset node (name key) val))
    tcoll))

(extend-type SvgProps
  ILookup
  (-lookup
    ([o k]
     (when-let [prop (aget (dom/-underlying o) (name k))]
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
     (or (-lookup o k) not-found))))

(defn props
  [node]
  (when node (SvgProps. node)))
