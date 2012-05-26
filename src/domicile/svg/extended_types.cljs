(ns domicile.svg.extended-types
  (:require
    [domicile.svg.core-defs :as cdef])
  (:use-macros
    [domicile.svg.list-macro :only [extend-svg-list]]))

(defn ^:private vec<-path-seg
  [seg]
  (let [type (. seg -pathSegTypeAsLetter)
        k (keyword type)]
    (condp #(some %2 %1) #{(. type toLowerCase)}
      "mlt" [k (. seg -x ) (. seg -y)]
      "z" [k]
      "h" [k (. seg -x)]
      "v" [k (. seg -y)]
      "c" [k (. seg -x) (. seg -y) (. seg -x1) (. seg -y1) (. seg -x2) (. seg -y2)]
      "s" [k (. seg -x) (. seg -y) (. seg -x2) (. seg -y2)]
      "q" [k (. seg -x) (. seg -y) (. seg -x1) (. seg -y1)]
      "a" [k (. seg -x) (. seg -y) (. seg -r1) (. seg -r2) (. seg -angle)
           (. seg -largeArcFlag) (. seg -sweepFlag)]
      (throw (js/Error. (str "Unknown SVGPathSeg type: " type))))))

(defn ^:private path-seg-count
  [seg]
  (let [type (. seg -pathSegTypeAsLetter)]
    (condp #(some %2 %1) #{(. type toLowerCase)}
      "mlt" 3
      "z" 1
      "hv" 2
      "c" 7
      "sq" 5
      "a" 8
      (throw (js/Error. (str "Unknown SVGPathSeg type: " type))))))

(extend-type js/SVGPoint
  ISeqable
  (-seq [pt] (-seq [(. pt -x) (. pt -y)]))

  ICounted
  (-count [pt] 2)

  ITransientAssociative
  (-assoc!
    [pt key val]
    (cond
      (#{:x 0} key) (set! (. pt -x) val)
      (#{:y 1} key) (set! (. pt -y) val)
      :else (throw (js/Error. (str "No such key for SVGPoint: " key))))
    pt)

  IIndexed
  (-nth
    ([pt n not-found]
     (condp = n
       0 (. pt -x)
       1 (. pt -y)
       :else nil))
    ([pt n]
     (or (-nth pt n nil)
         (throw (js/Error. "Two fields in a point.")))))

  ILookup
  (-lookup
    ([pt key not-found]
     (cond
       (#{:x 0} key) (. pt -x)
       (#{:y 1} key) (. pt -y)
       :else not-found))
    ([pt key]
     (or (-lookup pt key nil)
         (throw (js/Error. (str "No such key for SVGPoint: " key)))))))


(extend-type js/SVGRect
  ISeqable
  (-seq [rec] (-seq [(. rec -x) (. rec -y)
                     (. rec -width) (. rec -height)]))

  ICounted
  (-count [pt] 4)

  ITransientAssociative
  (-assoc!
    [rec key val]
    (cond
      (#{:x 0} key) (set! (. rec -x) val)
      (#{:y 1} key) (set! (. rec -y) val)
      (#{:w 2} key) (set! (. rec -width) val)
      (#{:h 3} key) (set! (. rec -height) val)
      :else (throw (js/Error. (str "No such key for SVGRect: " key))))
    rec)

  IIndexed
  (-nth
    ([rec n not-found]
     (condp = n
       0 (. rec -x)
       1 (. rec -y)
       2 (. rec -width)
       3 (. rec -height)
       :else not-found))
    ([pt n]
     (or (-nth rec n nil)
         (throw (js/Error. "Four fields in a rect.")))))

  ILookup
  (-lookup
    ([rec key not-found]
     (cond
       (#{:x 0} key) (. rec -x)
       (#{:y 1} key) (. rec -y)
       (#{:w 2} key) (. rec -width)
       (#{:h 3} key) (. rec -height)
       :else not-found))
    ([rec key]
     (or (-lookup rec key nil)
         (throw (js/Error. (str "No such key for SVGRect: " key)))))))


(extend-type js/SVGMatrix
  ISeqable
  (-seq [mx] (-seq [(. mx -a) (. mx -b)
                    (. mx -c) (. mx -d)
                    (. mx -e) (. mx -f)]))

  ICounted
  (-count [pt] 6)

  ITransientAssociative
  (-assoc!
    [mx key val]
    (cond
      (#{:a 0} key) (set! (. mx -a) val)
      (#{:b 1} key) (set! (. mx -b) val)
      (#{:c 2} key) (set! (. mx -c) val)
      (#{:d 3} key) (set! (. mx -d) val)
      (#{:e 4} key) (set! (. mx -e) val)
      (#{:f 5} key) (set! (. mx -f) val)
      :else (throw (js/Error. (str "No such key for SVGMatrix: " key))))
    mx)

  IIndexed
  (-nth
    ([mx n not-found]
     (condp = n
       0 (. mx -a)
       1 (. mx -b)
       2 (. mx -c)
       3 (. mx -d)
       4 (. mx -e)
       5 (. mx -f)
       :else not-found))
    ([pt n]
     (or (-nth mx n nil)
         (throw (js/Error. "Six fields in a matrix.")))))

  ILookup
  (-lookup
    ([mx key not-found]
     (cond
       (#{:a 0} key) (. mx -a)
       (#{:b 1} key) (. mx -b)
       (#{:c 2} key) (. mx -c)
       (#{:d 3} key) (. mx -d)
       (#{:e 4} key) (. mx -e)
       (#{:f 5} key) (. mx -f)
       :else not-found))
    ([mx key]
     (or (-lookup mx key nil)
         (throw (js/Error. (str "No such key for SVGMatrix: " key)))))))


(extend-type js/SVGTransform
  ISeqable
  (-seq
    [tr]
    (let [[a b c d e f :as mx] (. tr -matrix)
          type (. tr -type)]
      (condp = type
        1 (list* :matrix (seq mx))
        2 (list :translate e f)
        3 (list :scale a d)
        4 (list :rotate (. tr -angle) e f)
        5 (list :skew-x (. tr -angle))
        6 (list :skew-y (. tr -angle))
        (throw (js/Error. (str "Unknown SVGTransform type: " type)))))))


(extend-type js/SVGPathSeg
  ISeqable
  (-seq [seg] (seq (vec<-path-seg seg)))

  ICounted
  (-count [seg] (path-seg-count seg))

  ILookup
  (-lookup
    ([seg key]
     (if (= :type key)
       (keyword (. seg -pathSegTypeAsLetter))
       (aget seg (name key))))
    ([pt key not-found]
     (or (-lookup seg key) not-found))))


(extend-svg-list js/SVGLengthList cdef/length #(. % -value))
(extend-svg-list js/SVGNumberList cdef/number #(. % -value))
(extend-svg-list js/SVGPointList cdef/point identity)
(extend-svg-list js/SVGStringList identity identity)
(extend-svg-list js/SVGElementInstanceList identity identity)
(extend-svg-list js/SVGPathSegList cdef/path-seg identity)
(extend-svg-list js/SVGTransformList identity identity)
