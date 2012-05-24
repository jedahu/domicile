(ns domicile.svg
  (:require
    [domicile.core :as dom]))

(deftype SvgProps [elem]
  dom/Wrapper
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
    (throw (js/Error. "Deliberately not implemented.")))

  ITransientAssociative
  (-assoc!
    [tcoll key val]
    (if-let [prop (aget elem (name key))]
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

        :else (aset elem (name key) val))
      (aset elem (name key) val))
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
         prop

         (. prop -baseVal)
         (let [bv (. prop -baseVal)]
           (cond
             (. bv -value)         (. bv -value)
             (. bv -numberOfItems) bv
             :else                 bv))

         :else prop)))
    ([o k not-found]
     (or (-lookup o k) not-found))))

(defn props
  [elem]
  (when elem (SvgProps. elem)))
