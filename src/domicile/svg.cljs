(ns domicile.svg
  (:require
    [domicile.dom :as dom]
    [domicile.ns :as ns]
    [domicile.svg.core-defs :as cdef]
    [domicile.svg.extended-types :as _et]
    [clojure.string :as str])
  (:use
    [domicile.util :only [update!]]
    [domicile.create :only [get-document]])
  (:use-macros
    [domicile.util.macros :only [japply]]))

(def svg-root cdef/svg-root)

(def path-elem cdef/path-elem)

(def length cdef/length)

(def number cdef/number)

(def point cdef/point)

(def path-seg cdef/path-seg)

(defprotocol Wrapper
  (-underlying [wrapper]))

(defn underlying
  [wrapper]
  (-underlying wrapper))

(deftype SvgProps [elem]
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
     (when-let [prop (aget (-underlying o) (name k))]
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


(defn str<-path-seg
  [seg]
  (let [seg (path-seg seg)
        type (. seg -pathSegTypeAsLetter)]
    (str/join
      " "
      (condp #(some %2 %1) #{(. type toLowerCase)}
        "mlt" [type (. seg -x) (. seg -y)]
        "z" [type]
        "h" [type (. seg -x)]
        "v" [type (. seg -y)]
        "c" [type (. seg -x1) (. seg -y1) (. seg -x2) (. seg -y2) (. seg -x) (. seg y)]
        "s" [type (. seg -x2) (. seg -y2) (. seg -x) (. seg -y)]
        "q" [type (. seg -x1) (. seg -y1) (. seg -x) (. seg -y)]
        "a" [type (. seg -r1) (. seg -r2) (. seg -angle)
             (. seg -largeArcFlag) (. seg -sweepFlag) (. seg -x) (. seg -y)]
        (throw (js/Error. (str "Unknown SVGPathSeg type: " type)))))))


(defn point-x
  [pt]
  (if (instance? js/SVGPoint pt)
    (. pt -x)
    (first pt)))

(defn point-y
  [pt]
  (if (instance? js/SVGPoint pt)
    (. pt -y)
    (second pt)))

(defn pointwise
  [f & pts]
  [(apply f (map point-x pts))
   (apply f (map point-y pts))])

(defn distance
  [pt1 pt2]
  (let [[x1 y1] pt1
        [x2 y2] pt2
        dx (- x1 x2)
        dy (- y1 y2)]
    (. js/Math sqrt (+ (* dx dx) (* dy dy)))))

(defn rect
  ([]
   (. svg-root createSVGRect))
  ([x y w h]
   (let [rec (. svg-root createSVGRect)]
     (set! (. rec -x) x)
     (set! (. rec -y) y)
     (set! (. rec -width) w)
     (set! (. rec -height) h)
     rec))
  ([xywh]
   (if (instance? js/SVGRect xywh)
     xywh
     (apply rect xywh))))

(defn rect-points
  [rec]
  (let [[x y w h] rec]
    [[x y]
     [(+ x w) y]
     [(+ x w) (+ y h)]
     [x (+ y h)]]))

(defn rect-center
  [rec]
  (let [[x y w h] rec]
    [(+ x (/ w 2)) (+ y (/ h 2))]))

(defn matrix
  ([]
   (. svg-root createSVGMatrix))
  ([a b c d e f]
   (let [mx (. svg-root createSVGMatrix)]
     (set! (. mx -a) a)
     (set! (. mx -b) b)
     (set! (. mx -c) c)
     (set! (. mx -d) d)
     (set! (. mx -e) e)
     (set! (. mx -f) f)
     mx))
  ([abcdef]
   (if (instance? js/SVGMatrix abcdef)
     abcdef
     (apply matrix abcdef))))

(defn elem-mx
  [elem]
  (when-not (. elem getAttribute "transform")
    (. elem setAttribute "transform" "scale(1)"))
  (.. elem -transform -baseVal (consolidate) -matrix))

(defn mx-components
  [elem|mx]
  (let [[a b c d e f] (if (instance? js/SVGMatrix elem|mx)
                        elem|mx
                        (elem-mx elem|mx))]
    {:translation [e f]
     :rotation (Math/atan2 b a)
     :scale [(Math/sqrt (+ (* a a) (* b b)))
             (Math/sqrt (+ (* c c) (* d d)))]}))

(defn set-elem-mx!
  [elem mx]
  (let [ts (.. elem -transform -baseVal)
        t (. ts createSVGTransformFromMatrix (matrix mx))]
    (. ts initialize t)
    (.. ts (consolidate) -matrix)))

(defn elem*mx!
  [elem mx]
  (let [ts (.. elem -transform -baseVal)
        cur (elem-mx elem)
        t (. ts createSVGTransformFromMatrix (. cur multiply (matrix mx)))]
    (. ts initialize t)
    (.. ts (consolidate) -matrix)))

(defn mx*elem!
  [mx elem]
  (let [ts (.. elem -transform -baseVal)
        cur (elem-mx elem)
        t (. ts createSVGTransformFromMatrix (. (matrix mx) multiply cur))]
    (. ts initialize t)
    (.. ts (consolidate) -matrix)))

(defn clear-elem-mx!
  [elem]
  (let [mx (elem-mx elem)]
    (set! (. mx -a) 1)
    (set! (. mx -b) 0)
    (set! (. mx -c) 0)
    (set! (. mx -d) 1)
    (set! (. mx -e) 0)
    (set! (. mx -f) 0)
    (set-elem-mx! elem mx)
    mx))

(defn transform-point
  ([pt mx]
   (. (point pt) matrixTransform mx))
  ([pt from to]
   (transform-point pt (. from getTransformToElement to))))

(defn transform-rect
  ([rec mx]
   (vec
     (for [pt (rect-points rec)]
       (transform-point pt mx))))
  ([rec from to]
   (transform-rect rec (. from getTransformToElement to))))

(defprotocol CenterOrigin
  (-center-origin! [node xy]))

(defn- center-origin-xywh! [elem [x1 y1]]
  (let [props (props elem)
        {:keys [x y width height]} props
        cx (or x1 (/ width 2))
        cy (or y1 (/ height 2))]
    (conj! props {:x (- cx) :y (- cy)})
    (set-elem-mx!
      elem (. (elem-mx elem)
              translate (+ x cx) (+ y cy)))))

(defn- center-origin-xybb! [elem xy]
  (let [[x y _ _ :as bb] (. elem getBBox)
        [cx cy] (or xy (rect-center bb))]
    (conj! (props elem) {:x (- x cx) :y (- y cy)})
    (set-elem-mx!
      elem (. (elem-mx elem)
              translate cx cy))))

(defn- center-origin-text! [elem xy]
  (let [[x y w h :as bb] (. elem getBBox)
        [cx cy] (or xy (rect-center bb))]
    (if (= [0 0] [cx cy])
      (elem-mx elem)
      (let [w2 (/ w 2)
            h2 (/ h 2)
            dx (+ x w2)
            dy (+ y h2)
            x1 (.. elem -x -baseVal)
            y1 (.. elem -y -baseVal)]
        (if (seq x1)
          (update! x1 0 - dx)
          (conj! x1 (- dx)))
        (if (seq y1)
          (update! y1 0 - dy)
          (conj! y1 (- dy)))
        (set-elem-mx!
          elem (. (elem-mx elem)
                  translate dx dy))))))

(extend-protocol CenterOrigin

  js/SVGRectElement
  (-center-origin! [elem xy]
    (center-origin-xywh! elem xy))

  js/SVGImageElement
  (-center-origin! [elem xy]
    (center-origin-xywh! elem xy))

  ;js/SVGEllipseElement
  ;(-center-origin! [elem xy]
  ;  (let [props (props elem)
  ;        {:keys [cx cy]} props
  ;        [x y] (or xy [0 0])]
  ;    (conj! props {:cx (- cx x) :cy (- cy y)})
  ;    (set-elem-mx!
  ;      elem (. (elem-mx elem)
  ;              (translate (+ cx x) (+ cy y))))))

  ;js/SVGGElement
  ;(-center-origin! [elem xy]
  ;  (let [[cx cy] (or xy (rect-center (. elem getBBox)))]
  ;    (doseq [n (dom/dom-list (. elem -childNodes))]
  ;      (when (instance? js/Element n)
  ;        (set-elem-mx!
  ;          n (. (elem-mx n)
  ;               translate (- cx) (- cy)))))
  ;    (set-elem-mx!
  ;      elem (. (elem-mx elem)
  ;              translate cx cy))))

  ;js/SVGPolygonElement
  ;(-center-origin! [elem xy]
  ;  (let [[x y _ _ :as bb] (vec<-rect (. elem getBBox))
  ;        [cx cy] (or xy (rect-center bb))
  ;        pts (. node -points)
  ;        pts* (map pair<-point (svg/svg-list pts))]
  ;    (. pts clear)
  ;    (doseq [[x y] pts*]
  ;      (. pts appendItem (point (- x cx) (- y cy))))
  ;    (set-elem-mx!
  ;      elem (. (elem-mx elem)
  ;              translate cx cy))))

  js/SVGTextElement
  (-center-origin! [elem xy]
    (center-origin-text! elem xy)))

(defn center-origin!
  [node & [xy]]
  (-center-origin! node xy))

(defn with-g-wrap [elem f]
  (let [g (. (get-document) createElementNS ns/svgns "g")
        p (. elem -parentNode)]
    (. p insertBefore g elem)
    (. g appendChild elem)
    (let [ret (f g)]
      (. p insertBefore elem g)
      (. p removeChild g)
      ret)))

(defn rects-intersect?
  [rec1 rec2]
  (let [[x1 y1 w1 h1] rec1
        [x2 y2 w2 h2] rec2]
    (and (< x2 (+ x1 w1))
         (> (+ x2 w2) x1)
         (< y2 (+ y1 h1))
         (> (+ y2 h2) y1))))

(defn evt-client-point
  [evt]
  (point (. evt -clientX) (. evt -clientY)))
