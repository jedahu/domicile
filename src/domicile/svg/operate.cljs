(ns domicile.svg.operate
  (:require
    [domicile.core :as dom]
    [domicile.ns :as ns]
    [domicile.svg :as svg]
    [clojure.string :as str])
  (:use
    [domicile.create :only [get-document]])
  (:use-macros
    [domicile.macros :only [japply]]
    [domicile.svg.list-macro :only [extend-svg-list]]))

(def svg-root (. js/document createElementNS ns/svgns "svg"))

(def path-elem (. js/document createElementNS ns/svgns "path"))

(defn length
  ([]
   (. svg-root createSVGLength))
  ([n]
   (if (instance? js/SVGLength n)
     n
     (let [len (. svg-root createSVGLength)]
       (set! (. len -value) n)
       len))))

(defn number
  ([]
   (. svg-root createSVGNumber))
  ([n]
   (if (instance? js/SVGNumber n)
     n
     (let [len (. svg-root createSVGNumber)]
       (set! (. len -value) n)
       len))))

(defn point
  ([]
   (. svg-root createSVGPoint))
  ([x y]
   (let [pt (. svg-root createSVGPoint)]
     (set! (. pt -x) x)
     (set! (. pt -y) y)
     pt))
  ([xy]
   (if (instance? js/SVGPoint xy)
     xy
     (apply point xy))))

(defn path-seg
  [type|seg & args]
  (cond
    (or (keyword? type|seg)
        (string? type|seg))
    (condp = (keyword type|seg)
      :z (. path-elem createSVGPathSegClosePath)
      :M (japply path-elem createSVGPathSegMovetoAbs args)
      :m (japply path-elem createSVGPathSegMovetoRel args)
      :L (japply path-elem createSVGPathSegLinetoAbs args)
      :l (japply path-elem createSVGPathSegLinetoRel args)
      :C (japply path-elem createSVGPathSegCurvetoCubicAbs args)
      :c (japply path-elem createSVGPathSegCurvetoCubicRel args)
      :Q (japply path-elem createSVGPathSegCurvetoQuadraticAbs args)
      :q (japply path-elem createSVGPathSegCurvetoQuadraticRel args)
      :A (japply path-elem createSVGPathSegArcAbs args)
      :a (japply path-elem createSVGPathSegArcRel args)
      :H (japply path-elem createSVGPathSegLinetoHorizontalAbs args)
      :h (japply path-elem createSVGPathSegLinetoHorizontalRel args)
      :V (japply path-elem createSVGPathSegLinetoVerticalAbs args)
      :v (japply path-elem createSVGPathSegLinetoVerticalRel args)
      :S (japply path-elem createSVGPathSegCurvetoCubicSmoothAbs args)
      :s (japply path-elem createSVGPathSegCurvetoCubicSmoothRel args)
      :T (japply path-elem createSVGPathSegCurvetoQuadraticSmoothAbs args)
      :t (japply path-elem createSVGPathSegCurvetoQuadraticSmoothRel args)
      (throw (js/Error. (str "Unknown SVGPathSeg type: " type|seg))))

    (instance? js/SVGPathSeg type|seg) type|seg
    :else (apply path-seg type|seg)))

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


(extend-svg-list js/SVGLengthList length #(. % -value))
(extend-svg-list js/SVGNumberList number #(. % -value))
(extend-svg-list js/SVGPointList point identity)
(extend-svg-list js/SVGStringList identity identity)
(extend-svg-list js/SVGElementInstanceList identity identity)
(extend-svg-list js/SVGPathSegList path-seg identity)

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
  [pt mx]
  (. (point pt) matrixTransform mx))

(defn transform-rect
  [rec mx]
  (vec
    (for [pt (rect-points rec)]
      (transform-point pt mx))))

(defprotocol CenterOrigin
  (-center-origin! [node xy]))

(defn- center-origin-xywh! [elem [x1 y1]]
  (let [props (svg/props elem)
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
    (conj! (svg/props elem) {:x (- x cx) :y (- y cy)})
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
            dx (- x w2)
            dy (- y h2)
            x1 (.. elem -x -baseVal (getItem 0))
            y1 (.. elem -y -baseVal (getItem 0))]
        (set! (. x1 -value) (- (. x1 -value) (+ x w2)))
        (set! (. y1 -value) (- (. y1 -value) (+ y h2)))
        ;(set! (. x1 -value) (- (. x1 -value) cx))
        ;(set! (. y1 -value) (- (. y1 -value) cy))
        (set-elem-mx!
          elem (. (elem-mx elem)
                  translate (+ x w2) (+ y h2)))))))

(extend-protocol CenterOrigin

  js/SVGRectElement
  (-center-origin! [elem xy]
    (center-origin-xywh! elem xy))

  js/SVGImageElement
  (-center-origin! [elem xy]
    (center-origin-xywh! elem xy))

  ;js/SVGEllipseElement
  ;(-center-origin! [elem xy]
  ;  (let [props (svg/props elem)
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
