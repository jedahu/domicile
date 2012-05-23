(ns domicile.svg.operate
  (:require
    [domicile.ns :as ns])
  (:use
    [domicile.create :only [get-document]]))

(def svg-root (. js/document createElementNS ns/svgns "svg"))

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

(defn pair<-point
  [pt]
  (if (instance? js/SVGPoint pt)
    [(. pt -x) (. pt -y)]
    pt))

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
  (let [[x1 y1] (pair<-point pt1)
        [x2 y2] (pair<-point pt2)
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

(defn vec<-rect
  [rec]
  (if (instance? js/SVGRect rec)
    [(. rec -x) (. rec -y) (. rec -width) (. rec -height)]
    rec))

(defn rect-points
  [rec]
  (let [[x y w h] (vec<-rect rec)]
    [[x y]
     [(+ x w) y]
     [(+ x w) (+ y h)]
     [x (+ y h)]]))

(defn rect-center
  [rec]
  (let [[x y w h] (vec<-rect rec)]
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

(defn vec<-mx
  [mx]
  (if (instance? js/SVGMatrix mx)
    [(. mx -a) (. mx -b) (. mx -c) (. mx -d) (. mx -e) (. mx -f)]
    mx))

(defn elem-mx
  [elem]
  (when-not (. elem getAttribute "transform")
    (. elem setAttribute "transform" "scale(1)"))
  (.. elem -transform -baseVal (consolidate) -matrix))

(defn mx-components
  [elem|mx]
  (let [[a b c d e f] (vec<-mx
                        (if (instance? js/SVGMatrix elem|mx)
                          elem|mx
                          (elem-mx elem|mx)))]
    {:translation [e f]
     :rotation (Math/atan2 b a)
     :scale [(Math/sqrt (+ (* a a) (* b b)))
             (Math/sqrt (+ (* c c) (* d d)))]}))

(defn set-elem-mx
  [elem mx]
  (let [ts (.. elem -transform -baseVal)
        t (. ts createSVGTransformFromMatrix (matrix mx))]
    (. ts initialize t)
    (.. ts (consolidate) -matrix)))

(defn elem-mx-*
  [elem mx]
  (let [ts (.. elem -transform -baseVal)
        cur (elem-mx elem)
        t (. ts createSVGTransformFromMatrix (. cur multiply (matrix mx)))]
    (. ts initialize t)
    (.. ts (consolidate) -matrix)))

(defn mx-elem-*
  [mx elem]
  (let [ts (.. elem -transform -baseVal)
        cur (elem-mx elem)
        t (. ts createSVGTransformFromMatrix (. (matrix mx) multiply cur))]
    (. ts initialize t)
    (.. ts (consolidate) -matrix)))

(defn clear-elem-mx
  [elem]
  (let [mx (elem-mx elem)]
    (set! (. mx -a) 1)
    (set! (. mx -b) 0)
    (set! (. mx -c) 0)
    (set! (. mx -d) 1)
    (set! (. mx -e) 0)
    (set! (. mx -f) 0)
    (set-elem-mx elem mx)
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
  (-center-origin [node xy]))

(defn- center-origin-xywh [elem]
  (let [props (svg/props elem)
        {:keys [x y width height]} props
        cx (/ w 2)
        cy (/ h 2)]
    (conj! props {:x (- cx) :y (- cy)})
    (set-elem-mx
      elem (. (elem-mx elem)
              (translate (+ x cx) (+ y cy))))))

(defn- center-origin-xybb [elem xy]
  (let [[x y _ _ :as bb] (vec<-rect (. elem getBBox))
        [cx cy] (or xy (rect-center bb))]
    (conj! (svg/props elem) {:x (- x cx) :y (- y cy)})
    (set-elem-mx
      elem (. (elem-mx elem)
              (translate (+ cx) (+ cy))))))

(extend-protocol CenterOrigin

  js/SVGRectElement
  (-center-origin [elem xy]
    (center-origin-xybb elem xy))

  js/SVGEllipseElement
  (-center-origin [elem xy]
    (let [props (svg/props elem)
          {:keys [cx cy]} props
          [x y] (or xy [0 0])]
      (conj! props {:cx (- cx x) :cy (- cy y)})
      (set-elem-mx
        elem (. (elem-mx elem)
                (translate (+ cx x) (+ cy y))))))

  js/SVGGElement
  (-center-origin [elem xy]
    (let [[cx cy] (or xy (rect-center (. elem getBBox)))]
      (doseq [n (dom/dom-list (. elem -childNodes))]
        (when (instance? js/Element n)
          (set-elem-mx
            n (. (elem-mx n)
                 (translate (- cx) (- cy))))))
      (set-elem-mx
        elem (. (elem-mx elem)
                (translate cx cy)))))

  js/SVGPolygonElement
  (-center-origin [elem xy]
    (let [[x y _ _ :as bb] (vec<-rect (. elem getBBox))
          [cx cy] (or xy (rect-center bb))
          pts (. node -points)
          pts* (map pair<-point (svg/svg-list pts))]
      (. pts clear)
      (doseq [[x y] pts*]
        (. pts appendItem (point (- x cx) (- y cy))))
      (set-elem-mx
        elem (. (elem-mx elem)
                (translate cx cy)))))

  js/SVGTextElement
  (-center-origin [elem xy]
    ; Why is bbox.y incorrect?
    (with-g-wrap node
      #(-center-origin % xy))
    #_(let [x (.. elem -x -baseVal (getItem 0) -value)
          y (.. elem -y -baseVal (getItem 0) -value)
          [bx by _ _ :as bb] (vec<-rect (. elem getBBox))
          [cx cy] (or xy (rect-center bb))]
      (set! (.. elem -x -baseVal (getItem 0) -value) (- x cx))
      (set! (.. elem -y -baseVal (getItem 0) -value) (- y cy))
      (set-elem-mx
        elem (. (elem-mx elem)
                (translate (+ x cx) (+ y cy)))))))

(defn center-origin
  "Modify and transform `node` so its origin of transformation is the center of
  its BBox."
  [node & [xy]]
  (-center-origin node xy))

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
  (let [[x1 y1 w1 h1] (vec<-rect rec1)
        [x2 y2 w2 h2] (vec<-rect rec2)]
    (and (< x2 (+ x1 w1))
         (> (+ x2 w2) x1)
         (< y2 (+ y1 h1))
         (> (+ y2 h2) y1))))
