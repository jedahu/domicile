(ns domicile.svg-test
  (:require
    [domicile.svg :as svg]
    [domicile.ns :as ns]
    [menodora.core :as mc])
  (:use
    [menodora.predicates :only [eq type-eq truthy]])
  (:use-macros
    [menodora :only [defsuite describe should expect]]))

(defsuite svg-tests
  (describe "SvgProps"
    (should "implement ILookup"
      ;; prop -baseVal -value
      (let [node (. js/document createElementNS ns/svgns "rect")
            props (svg/props node)]
        (set! (.. node -x -baseVal -value) 10)
        (expect eq 10 (:x props))
        (expect eq nil (:bar props))
        (expect eq nil (get props [:not :string :or :keyword]))
        (expect eq 10 (get props :x :not-found))
        (expect eq :not-found (get props :bar :not-found)))
      ;; prop -baseVal
      (let [node (. js/document createElementNS ns/svgns "path")
            props (svg/props node)]
        (. node setAttribute "pathLength" "3")
        (expect eq 3 (:pathLength props)))
      ;; prop -baseVal (list)
      (let [node (. js/document createElementNS ns/svgns "polyline")
            props (svg/props node)]
        (. node setAttribute "points" "0 0 100 100 20 80")
        (expect type-eq js/SVGPointList (:points props))
        (expect eq 3 (count (:points props)))))
    (should "implement ITransientCollection"
      (let [node (. js/document createElementNS ns/svgns "rect")
            props (svg/props node)]
        (expect eq props (conj! props [:x 10]))
        (expect eq 10 (.. node -x -baseVal -value))
        (expect eq props (conj! props {:x 11 :y 22}))
        (expect eq 11 (.. node -x -baseVal -value))
        (expect eq 22 (.. node -y -baseVal -value))))
    (should "implement ITransientAssociative"
      (let [node (. js/document createElementNS ns/svgns "rect")
            props (svg/props node)]
        (expect eq props (assoc! props :x 10))
        (expect eq 10 (.. node -x -baseVal -value))
        (assoc! props :x 11)
        (expect eq 11 (.. node -x -baseVal -value)))))

  (describe "point"
    (should "take no args"
      (expect type-eq js/SVGPoint (svg/point))
      (expect eq 0 (. (svg/point) -x))
      (expect eq 0 (. (svg/point) -y)))
    (should "take x y args"
      (let [pt (svg/point 1 2)]
        (expect type-eq js/SVGPoint pt)
        (expect eq 1 (. pt -x))
        (expect eq 2 (. pt -y))))
    (should "take pair arg"
      (let [pt (svg/point [1 2])]
        (expect type-eq js/SVGPoint pt)
        (expect eq 1 (. pt -x))
        (expect eq 2 (. pt -y))))
    (should "take SVGPoint arg"
      (let [pt (. svg/svg-root createSVGPoint)]
        (set! (. pt -x) 1)
        (set! (. pt -y) 2)
        (expect type-eq js/SVGPoint (svg/point pt))
        (expect eq pt (svg/point pt)))))

  (describe "rect"
    (should "take no args"
      (expect type-eq js/SVGRect (svg/rect))
      (expect eq 0 (. (svg/rect) -x))
      (expect eq 0 (. (svg/rect) -y))
      (expect eq 0 (. (svg/rect) -width))
      (expect eq 0 (. (svg/rect) -height)))
    (should "take x y w h args"
      (let [rec (svg/rect 1 2 3 4)]
        (expect type-eq js/SVGRect rec)
        (expect eq 1 (. rec -x))
        (expect eq 2 (. rec -y))
        (expect eq 3 (. rec -width))
        (expect eq 4 (. rec -height))))
    (should "take sequence arg"
      (let [rec (svg/rect [1 2 3 4])]
        (expect type-eq js/SVGRect rec)
        (expect eq 1 (. rec -x))
        (expect eq 2 (. rec -y))
        (expect eq 3 (. rec -width))
        (expect eq 4 (. rec -height))))
    (should "take SVGRect arg"
      (let [rec (. svg/svg-root createSVGRect)]
        (set! (. rec -x) 1)
        (set! (. rec -y) 2)
        (set! (. rec -width) 3)
        (set! (. rec -height) 4)
        (expect type-eq js/SVGRect (svg/rect rec))
        (expect eq rec (svg/rect rec)))))

  (describe "matrix"
    (should "take no args"
      (expect type-eq js/SVGMatrix (svg/matrix))
      (expect eq 1 (. (svg/matrix) -a))
      (expect eq 0 (. (svg/matrix) -b))
      (expect eq 0 (. (svg/matrix) -c))
      (expect eq 1 (. (svg/matrix) -d))
      (expect eq 0 (. (svg/matrix) -e))
      (expect eq 0 (. (svg/matrix) -f)))
    (should "take a b c d e f args"
      (let [mx (svg/matrix 1 2 3 4 5 6)]
        (expect type-eq js/SVGMatrix mx)
        (expect eq 1 (. mx -a))
        (expect eq 2 (. mx -b))
        (expect eq 3 (. mx -c))
        (expect eq 4 (. mx -d))
        (expect eq 5 (. mx -e))
        (expect eq 6 (. mx -f))))
    (should "take sequence arg"
      (let [mx (svg/matrix [1 2 3 4 5 6])]
        (expect type-eq js/SVGMatrix mx)
        (expect eq 1 (. mx -a))
        (expect eq 2 (. mx -b))
        (expect eq 3 (. mx -c))
        (expect eq 4 (. mx -d))
        (expect eq 5 (. mx -e))
        (expect eq 6 (. mx -f))))
    (should "take SVGMatrix arg"
      (let [mx (. svg/svg-root createSVGMatrix)]
        (set! (. mx -a) 1)
        (set! (. mx -b) 2)
        (set! (. mx -c) 3)
        (set! (. mx -d) 4)
        (set! (. mx -e) 5)
        (set! (. mx -f) 6)
        (expect type-eq js/SVGMatrix (svg/matrix mx))
        (expect eq mx (svg/matrix mx)))))

  (describe "transform"
    (should "take no args"
      (expect type-eq js/SVGTransform (svg/transform)))
    (should "take args"
      (expect type-eq js/SVGTransform (svg/transform :translate 1 2)))
    (should "create matrix"
      (let [tr (svg/transform :matrix 1 2 3 4 5 6)
            mx (. tr -matrix)]
        (expect eq 1 (. tr -type))
        (expect eq 1 (. mx -a))
        (expect eq 2 (. mx -b))
        (expect eq 3 (. mx -c))
        (expect eq 4 (. mx -d))
        (expect eq 5 (. mx -e))
        (expect eq 6 (. mx -f))))
    (should "create translation"
      (let [tr (svg/transform :translate 3 4)]
        (expect eq 2 (. tr -type))
        (expect eq 3 (.. tr -matrix -e))
        (expect eq 4 (.. tr -matrix -f))))
    (should "create scale"
      (let [tr (svg/transform :scale 3 4)]
        (expect eq 3 (. tr -type))
        (expect eq 3 (.. tr -matrix -a))
        (expect eq 4 (.. tr -matrix -d))))
    (should "create rotation"
      (let [tr (svg/transform :rotate 90 3 8)]
        (expect eq 4 (. tr -type))
        (expect eq 90 (. tr -angle))
        (expect eq 11 (.. tr -matrix -e))
        (expect eq 5 (.. tr -matrix -f))))
    (should "create skewX"
      (let [tr (svg/transform :skew-x 30)]
        (expect eq 5 (. tr -type))
        (expect eq 30 (. tr -angle))))
    (should "create skewY"
      (let [tr (svg/transform :skew-y 30)]
        (expect eq 6 (. tr -type))
        (expect eq 30 (. tr -angle)))))

  (describe "path-seg"
    (should "take args"
      (let [seg (svg/path-seg :L 1 2)]
        (expect type-eq js/SVGPathSegLinetoAbs seg)
        (expect eq 1 (. seg -x))
        (expect eq 2 (. seg -y))))
    (should "take vector arg"
      (let [seg (svg/path-seg [:L 1 2])]
        (expect type-eq js/SVGPathSegLinetoAbs seg)
        (expect eq 1 (. seg -x))
        (expect eq 2 (. seg -y))))
    (should "take SVGPathSeg arg"
      (let [seg (. svg/path-elem createSVGPathSegLinetoAbs 1 2)]
        (expect eq seg (svg/path-seg seg))))
    (should "create SVGPathSegClosePath"
      (let [seg (svg/path-seg :z)]
        (expect type-eq js/SVGPathSegClosePath seg)))
    (should "create SVGPathSegMovetoAbs"
      (let [seg (svg/path-seg :M 1 2)]
        (expect type-eq js/SVGPathSegMovetoAbs seg)
        (expect eq 1 (. seg -x))
        (expect eq 2 (. seg -y))))
    (should "create SVGPathSegMovetoRel"
      (let [seg (svg/path-seg :m 1 2)]
        (expect type-eq js/SVGPathSegMovetoRel seg)
        (expect eq 1 (. seg -x))
        (expect eq 2 (. seg -y))))
    (should "create SVGPathSegLinetoAbs"
      (let [seg (svg/path-seg :L 1 2)]
        (expect type-eq js/SVGPathSegLinetoAbs seg)
        (expect eq 1 (. seg -x))
        (expect eq 2 (. seg -y))))
    (should "create SVGPathSegLinetoRel"
      (let [seg (svg/path-seg :l 1 2)]
        (expect type-eq js/SVGPathSegLinetoRel seg)
        (expect eq 1 (. seg -x))
        (expect eq 2 (. seg -y))))
    (should "create SVGPathSegCurvetoCubicAbs"
      (let [seg (svg/path-seg :C 1 2 3 4 5 6)]
        (expect type-eq js/SVGPathSegCurvetoCubicAbs seg)
        (expect eq 1 (. seg -x))
        (expect eq 2 (. seg -y))
        (expect eq 3 (. seg -x1))
        (expect eq 4 (. seg -y1))
        (expect eq 5 (. seg -x2))
        (expect eq 6 (. seg -y2))))
    (should "create SVGPathSegCurvetoCubicRel"
      (let [seg (svg/path-seg :c 1 2 3 4 5 6)]
        (expect type-eq js/SVGPathSegCurvetoCubicRel seg)
        (expect eq 1 (. seg -x))
        (expect eq 2 (. seg -y))
        (expect eq 3 (. seg -x1))
        (expect eq 4 (. seg -y1))
        (expect eq 5 (. seg -x2))
        (expect eq 6 (. seg -y2))))
    (should "create SVGPathSegCurvetoQuadraticAbs"
      (let [seg (svg/path-seg :Q 1 2 3 4)]
        (expect type-eq js/SVGPathSegCurvetoQuadraticAbs seg)
        (expect eq 1 (. seg -x))
        (expect eq 2 (. seg -y))
        (expect eq 3 (. seg -x1))
        (expect eq 4 (. seg -y1))))
    (should "create SVGPathSegCurvetoQuadraticRel"
      (let [seg (svg/path-seg :q 1 2 3 4)]
        (expect type-eq js/SVGPathSegCurvetoQuadraticRel seg)
        (expect eq 1 (. seg -x))
        (expect eq 2 (. seg -y))
        (expect eq 3 (. seg -x1))
        (expect eq 4 (. seg -y1))))
    (should "create SVGPathSegArcAbs"
      (let [seg (svg/path-seg :A 1 2 3 4 5 true true)]
        (expect type-eq js/SVGPathSegArcAbs seg)
        (expect eq 1 (. seg -x))
        (expect eq 2 (. seg -y))
        (expect eq 3 (. seg -r1))
        (expect eq 4 (. seg -r2))
        (expect eq 5 (. seg -angle))
        (expect eq true (. seg -largeArcFlag))
        (expect eq true (. seg -sweepFlag))))
    (should "create SVGPathSegArcRel"
      (let [seg (svg/path-seg :a 1 2 3 4 5 true true)]
        (expect type-eq js/SVGPathSegArcRel seg)
        (expect eq 1 (. seg -x))
        (expect eq 2 (. seg -y))
        (expect eq 3 (. seg -r1))
        (expect eq 4 (. seg -r2))
        (expect eq 5 (. seg -angle))
        (expect eq true (. seg -largeArcFlag))
        (expect eq true (. seg -sweepFlag))))
    (should "create SVGPathSegLinetoHorizontalAbs"
      (let [seg (svg/path-seg :H 9)]
        (expect type-eq js/SVGPathSegLinetoHorizontalAbs seg)
        (expect eq 9 (. seg -x))))
    (should "create SVGPathSegLinetoHorizontalRel"
      (let [seg (svg/path-seg :h 9)]
        (expect type-eq js/SVGPathSegLinetoHorizontalRel seg)
        (expect eq 9 (. seg -x))))
    (should "create SVGPathSegLinetoVerticalAbs"
      (let [seg (svg/path-seg :V 9)]
        (expect type-eq js/SVGPathSegLinetoVerticalAbs seg)
        (expect eq 9 (. seg -y))))
    (should "create SVGPathSegLinetoVerticalRel"
      (let [seg (svg/path-seg :v 9)]
        (expect type-eq js/SVGPathSegLinetoVerticalRel seg)
        (expect eq 9 (. seg -y))))
    (should "create SVGPathSegCurvetoCubicSmoothAbs"
      (let [seg (svg/path-seg :S 1 2 3 4)]
        (expect type-eq js/SVGPathSegCurvetoCubicSmoothAbs seg)
        (expect eq 1 (. seg -x))
        (expect eq 2 (. seg -y))
        (expect eq 3 (. seg -x2))
        (expect eq 4 (. seg -y2))))
    (should "create SVGPathSegCurvetoCubicSmoothRel"
      (let [seg (svg/path-seg :s 1 2 3 4)]
        (expect type-eq js/SVGPathSegCurvetoCubicSmoothRel seg)
        (expect eq 1 (. seg -x))
        (expect eq 2 (. seg -y))
        (expect eq 3 (. seg -x2))
        (expect eq 4 (. seg -y2))))
    (should "create SVGPathSegCurvetoQuadraticSmoothAbs"
      (let [seg (svg/path-seg :T 1 2)]
        (expect type-eq js/SVGPathSegCurvetoQuadraticSmoothAbs seg)
        (expect eq 1 (. seg -x))
        (expect eq 2 (. seg -y))))
    (should "create SVGPathSegCurvetoQuadraticSmoothRel"
      (let [seg (svg/path-seg :t 1 2)]
        (expect type-eq js/SVGPathSegCurvetoQuadraticSmoothRel seg)
        (expect eq 1 (. seg -x))
        (expect eq 2 (. seg -y)))))

  (describe "SVGLengthList"
    :let [lengths #(let [node (. js/document createElementNS ns/svgns "text")]
                     (. node setAttribute "x" "1 2 3 4")
                     (.. node -x -baseVal))]
    (should "implement ISeqable"
      (expect eq (seq [1 2 3 4]) (seq (lengths))))
    (should "implement ICounted"
      (expect eq 4 (count (lengths))))
    (should "implement IReduce"
      (expect eq 10 (reduce + (lengths)))
      (expect eq 20 (reduce + 10 (lengths))))
    (should "implememnt IIndexed"
      (expect eq 3 (nth (lengths) 2))
      (expect eq 2 (nth (lengths) 1 :not-found))
      (expect eq :not-found (nth (lengths) 4 :not-found)))
    (should "implement ITransientCollection"
      (let [ls (lengths)]
        (expect eq ls (conj! ls 5))
        (expect eq 5 (.. ls (getItem 4) -value))
        (expect type-eq PersistentVector (persistent! ls))
        (expect eq [1 2 3 4 5] (vec (persistent! ls)))))
    (should "implement ITransientAssociative"
      (let [ls (lengths)]
        (expect eq ls (assoc! ls 0 9))
        (expect eq 9 (.. ls (getItem 0) -value))))
    (should "implement ITransientVector"
      (let [ls (lengths)]
        (expect eq ls (pop! ls))
        (expect eq 3 (count ls))
        (expect eq (seq [1 2 3]) (seq ls)))))

  (describe "SVGPointList"
    :let [points #(let [node (. js/document createElementNS ns/svgns "polygon")]
                    (. node setAttribute "points" "0 0 100 100 20 80")
                    (. node -points))
          point (let [pt (. svg/svg-root createSVGPoint)]
                  (set! (. pt -x) 3)
                  (set! (. pt -y) 9)
                  pt)]
    (should "implement ISeqable"
      (expect eq (seq [[0 0] [100 100] [20 80]])
        (for [p (points)] [(. p -x) (. p -y)])))
    (should "implement ICounted"
      (expect eq 3 (count (points))))
    (should "implement IReduce"
      (expect eq [121 181] (reduce (fn [[x y] p] [(+ x (. p -x)) (+ y (. p -y))]) [1 1] (points))))
    (should "implememnt IIndexed"
      (expect eq 100 (. (nth (points) 1) -x))
      (expect type-eq js/SVGPoint (nth (points) 1 :not-found))
      (expect eq :not-found (nth (points) 3 :not-found)))
    (should "implement ITransientCollection"
      (let [pts (points)]
        (expect eq pts (conj! pts point))
        (expect eq point (. pts getItem 3))
        (expect type-eq PersistentVector (persistent! pts))
        (expect eq [[0 0] [100 100] [20 80] [3 9]]
          (vec (map (fn [p] [(. p -x) (. p -y)]) (persistent! pts))))))
    (should "implement ITransientAssociative"
      (let [pts (points)]
        (expect eq pts (assoc! pts 0 point))
        (expect eq point (. pts getItem 0))))
    (should "implement ITransientVector"
      (let [pts (points)]
        (expect eq pts (pop! pts))
        (expect eq 2 (count pts)))))

  (describe "point-x"
    (should "return a point's x value"
      (let [pt (. svg/svg-root createSVGPoint)]
        (set! (. pt -x) 9)
        (expect eq 9 (svg/point-x pt))
        (expect eq 9 (svg/point-x [9 0]))
        (expect eq 9 (svg/point-x '(9 0))))))

  (describe "point-y"
    (should "return a point's y value"
      (let [pt (. svg/svg-root createSVGPoint)]
        (set! (. pt -y) 8)
        (expect eq 8 (svg/point-y pt))
        (expect eq 8 (svg/point-y [0 8]))
        (expect eq 8 (svg/point-y '(0 8))))))

  (describe "pointwise"
    (should "apply a function pointwise"
      (expect eq [6 9] (svg/pointwise + [1 2] [2 3] [3 4]))))

  (describe "distance"
    (should "calculate the distance between two points"
      (expect eq 5 (svg/distance [0 3] [4 0]))
      (expect eq 13 (svg/distance [0 5] [12 0]))
      (expect eq 17 (svg/distance [0 8] [15 0]))
      (expect eq 25 (svg/distance [7 0] [0 24]))))

  (describe "rect-points"
    (should "return a vector of a rect's corner points in clockwise order"
      (expect eq [[3 8] [14 8] [14 27] [3 27]]
        (svg/rect-points [3 8 11 19]))))

  (describe "rect-center"
    (should "return center point of rect"
      (expect eq [8.5 17.5] (svg/rect-center [3 8 11 19]))))

  (describe "elem-mx"
    (should "return an SVG element's transform matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (. rec setAttribute "transform" "matrix(1, 2, 3, 4, 5, 6)")
        (expect type-eq js/SVGMatrix (svg/elem-mx rec))
        (expect eq [1 2 3 4 5 6] (vec (svg/elem-mx rec)))))
    (should "return an identity matrix if element has none"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (expect type-eq js/SVGMatrix (svg/elem-mx rec))
        (expect eq [1 0 0 1 0 0] (vec (svg/elem-mx rec))))))

  (describe "mx-components"
    (should "return a map of matrix components"
      (let [mx (. svg/svg-root createSVGMatrix)
            rec (. js/document createElementNS ns/svgns "rect")]
        (set! (. mx -a) -4)
        (set! (. mx -b) 0)
        (set! (. mx -c) 0)
        (set! (. mx -d) 0.5)
        (set! (. mx -e) 10)
        (set! (. mx -f) -20)
        (. rec setAttribute "transform" "matrix(-4, 0, 0, 0.5, 10, -20)")
        (expect eq {:translation [10 -20]
                    :rotation (. js/Math -PI)
                    :scale [4 0.5]}
          (svg/mx-components mx))
        (expect eq {:translation [10 -20]
                    :rotation (. js/Math -PI)
                    :scale [4 0.5]}
          (svg/mx-components rec)))))

  (describe "set-elem-mx!"
    (should "set an SVG element's transform matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (svg/set-elem-mx! rec [1 2 3 4 5 6])
        (expect eq [1 2 3 4 5 6]
          (vec (.. rec -transform -baseVal (consolidate) -matrix)))))
    (should "return the set matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (expect eq (vec (svg/set-elem-mx! rec [1 2 3 4 5 6]))
          (vec (.. rec -transform -baseVal (consolidate) -matrix))))))

  (describe "elem*mx!"
    (should "multiply an element's matrix by another matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (. rec setAttribute "transform" "matrix(-4, 0, 0, 0.5, 10, -20)")
        (svg/elem*mx! rec [1 0 0 1 2 0.5])
        (expect eq [-4, 0, 0, 0.5, 2, -19.75]
          (vec (.. rec -transform -baseVal (consolidate) -matrix)))))
    (should "return the updated matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (. rec setAttribute "transform" "matrix(-4, 0, 0, 0.5, 10, -20)")
        (expect eq (vec (svg/elem*mx! rec [1 0 0 1 2 0.5]))
          (vec (.. rec -transform -baseVal (consolidate) -matrix))))))

  (describe "mx*elem!"
    (should "multiply another matrix by an element's matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (. rec setAttribute "transform" "matrix(-4, 0, 0, 0.5, 10, -20)")
        (svg/mx*elem! [1 0 0 1 2 0.5] rec)
        (expect eq [-4, 0, 0, 0.5, 12, -19.5]
          (vec (.. rec -transform -baseVal (consolidate) -matrix)))))
    (should "return the updated matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (. rec setAttribute "transform" "matrix(-4, 0, 0, 0.5, 10, -20)")
        (expect eq (vec (svg/mx*elem! [1 0 0 1 2 0.5] rec))
          (vec (.. rec -transform -baseVal (consolidate) -matrix))))))

  (describe "clear-elem-mx!"
    :let [rec #(let [rec (. js/document createElementNS ns/svgns "rect")]
                 (. rec setAttribute "transform" "matrix(1, 2, 3, 4, 5, 6)")
                 rec)]
    (should "set elem's transform matrix to the identity matrix"
      (let [rec (rec)]
        (svg/clear-elem-mx! rec)
        (expect eq [1 0 0 1 0 0]
          (vec (.. rec -transform -baseVal (consolidate) -matrix)))))
    (should "return the new identity matrix"
      (let [rec (rec)]
        (expect eq (vec (svg/clear-elem-mx! rec))
          (vec (.. rec -transform -baseVal (consolidate) -matrix))))))

  (describe "transform-point"
    (should "transform a point by a matrix"
      (expect eq [6 10]
        (vec (svg/transform-point [3 2] (svg/matrix 1 0 0 1 3 8))))))

  (describe "transform-rect"
    (should "transform a rect by a matrix"
      (expect eq [[5 4] [8 4] [16 12] [13 12]]
        (map vec (svg/transform-rect [1 2 3 4] (svg/matrix 1 0 2 2 0 0))))))

  (describe "with-g-wrap"
    :let [a (. js/document createElementNS ns/svgns "g")
          b (. js/document createElementNS ns/svgns "rect")]
    :before (. a appendChild b)
    (should "wrap an SVG element in a group for the duration of f"
      (expect eq "g" (svg/with-g-wrap b (fn [g] (. g -tagName))))
      (expect eq b (svg/with-g-wrap b (fn [g] (. g -firstElementChild))))))

  (describe "rects-intersect?"
    (should "return true iff two rects intersect"
      (expect truthy (not (svg/rects-intersect? [0 0 1 1] [1 1 1 1])))
      (expect truthy (svg/rects-intersect? [0 0 9 9] [8 8 100 100]))
      (expect truthy (svg/rects-intersect? [0 0 9 9] [-8 -8 9 9]))
      (expect truthy (svg/rects-intersect? [0 0 9 9] [8 -8 100 9]))
      (expect truthy (svg/rects-intersect? [0 0 9 9] [-8 8 9 100])))))

;;. vim: set lispwords+=defsuite,describe,should,expect:
