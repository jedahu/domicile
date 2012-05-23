(ns domicile.svg.operate-test
  (:require
    [domicile.svg.operate :as op]
    [domicile.ns :as ns]
    [menodora.core :as mc])
  (:use
    [menodora.predicates :only [eq type-eq truthy]])
  (:use-macros
    [menodora :only [defsuite describe should expect]]))

(defsuite operate-tests
  (describe "point"
    (should "take no args"
      (expect type-eq js/SVGPoint (op/point))
      (expect eq 0 (. (op/point) -x))
      (expect eq 0 (. (op/point) -y)))
    (should "take x y args"
      (let [pt (op/point 1 2)]
        (expect type-eq js/SVGPoint pt)
        (expect eq 1 (. pt -x))
        (expect eq 2 (. pt -y))))
    (should "take pair arg"
      (let [pt (op/point [1 2])]
        (expect type-eq js/SVGPoint pt)
        (expect eq 1 (. pt -x))
        (expect eq 2 (. pt -y))))
    (should "take SVGPoint arg"
      (let [pt (. op/svg-root createSVGPoint)]
        (set! (. pt -x) 1)
        (set! (. pt -y) 2)
        (expect type-eq js/SVGPoint (op/point pt))
        (expect eq pt (op/point pt)))))

  (describe "pair<-point"
    (should "convert an SVGPoint to a pair of numbers"
      (let [pt (. op/svg-root createSVGPoint)]
        (set! (. pt -x) 1)
        (set! (. pt -y) 2)
        (expect eq [1 2] (op/pair<-point pt))))
    (should "return non SVGPoint arg unchanged"
      (expect eq [3 4] (op/pair<-point [3 4]))
      (expect eq :not-a-point (op/pair<-point :not-a-point))))

  (describe "point-x"
    (should "return a point's x value"
      (let [pt (. op/svg-root createSVGPoint)]
        (set! (. pt -x) 9)
        (expect eq 9 (op/point-x pt))
        (expect eq 9 (op/point-x [9 0]))
        (expect eq 9 (op/point-x '(9 0))))))

  (describe "point-y"
    (should "return a point's y value"
      (let [pt (. op/svg-root createSVGPoint)]
        (set! (. pt -y) 8)
        (expect eq 8 (op/point-y pt))
        (expect eq 8 (op/point-y [0 8]))
        (expect eq 8 (op/point-y '(0 8))))))

  (describe "pointwise"
    (should "apply a function pointwise"
      (expect eq [6 9] (op/pointwise + [1 2] [2 3] [3 4]))))

  (describe "distance"
    (should "calculate the distance between two points"
      (expect eq 5 (op/distance [0 3] [4 0]))
      (expect eq 13 (op/distance [0 5] [12 0]))
      (expect eq 17 (op/distance [0 8] [15 0]))
      (expect eq 25 (op/distance [7 0] [0 24]))))

  (describe "vec<-rect"
    (should "convert an SVGRect to a vector of numbers"
      (let [rec (. op/svg-root createSVGRect)]
        (set! (. rec -x) 1)
        (set! (. rec -y) 2)
        (set! (. rec -width) 3)
        (set! (. rec -height) 4)
        (expect eq [1 2 3 4] (op/vec<-rect rec))))
    (should "return non SVGRect arg unchanged"
      (expect eq [1 2 3 4] (op/vec<-rect [1 2 3 4]))
      (expect eq :not-a-point (op/vec<-rect :not-a-point))))

  (describe "rect-points"
    (should "return a vector of a rect's corner points in clockwise order"
      (expect eq [[3 8] [14 8] [14 27] [3 27]]
        (op/rect-points [3 8 11 19]))))

  (describe "rect-center"
    (should "return center point of rect"
      (expect eq [8.5 17.5] (op/rect-center [3 8 11 19]))))

  (describe "rect"
    (should "take no args"
      (expect type-eq js/SVGRect (op/rect))
      (expect eq 0 (. (op/rect) -x))
      (expect eq 0 (. (op/rect) -y))
      (expect eq 0 (. (op/rect) -width))
      (expect eq 0 (. (op/rect) -height)))
    (should "take x y w h args"
      (let [rec (op/rect 1 2 3 4)]
        (expect type-eq js/SVGRect rec)
        (expect eq 1 (. rec -x))
        (expect eq 2 (. rec -y))
        (expect eq 3 (. rec -width))
        (expect eq 4 (. rec -height))))
    (should "take sequence arg"
      (let [rec (op/rect [1 2 3 4])]
        (expect type-eq js/SVGRect rec)
        (expect eq 1 (. rec -x))
        (expect eq 2 (. rec -y))
        (expect eq 3 (. rec -width))
        (expect eq 4 (. rec -height))))
    (should "take SVGRect arg"
      (let [rec (. op/svg-root createSVGRect)]
        (set! (. rec -x) 1)
        (set! (. rec -y) 2)
        (set! (. rec -width) 3)
        (set! (. rec -height) 4)
        (expect type-eq js/SVGRect (op/rect rec))
        (expect eq rec (op/rect rec)))))

  (describe "matrix"
    (should "take no args"
      (expect type-eq js/SVGMatrix (op/matrix))
      (expect eq 1 (. (op/matrix) -a))
      (expect eq 0 (. (op/matrix) -b))
      (expect eq 0 (. (op/matrix) -c))
      (expect eq 1 (. (op/matrix) -d))
      (expect eq 0 (. (op/matrix) -e))
      (expect eq 0 (. (op/matrix) -f)))
    (should "take a b c d e f args"
      (let [mx (op/matrix 1 2 3 4 5 6)]
        (expect type-eq js/SVGMatrix mx)
        (expect eq 1 (. mx -a))
        (expect eq 2 (. mx -b))
        (expect eq 3 (. mx -c))
        (expect eq 4 (. mx -d))
        (expect eq 5 (. mx -e))
        (expect eq 6 (. mx -f))))
    (should "take sequence arg"
      (let [mx (op/matrix [1 2 3 4 5 6])]
        (expect type-eq js/SVGMatrix mx)
        (expect eq 1 (. mx -a))
        (expect eq 2 (. mx -b))
        (expect eq 3 (. mx -c))
        (expect eq 4 (. mx -d))
        (expect eq 5 (. mx -e))
        (expect eq 6 (. mx -f))))
    (should "take SVGMatrix arg"
      (let [mx (. op/svg-root createSVGMatrix)]
        (set! (. mx -a) 1)
        (set! (. mx -b) 2)
        (set! (. mx -c) 3)
        (set! (. mx -d) 4)
        (set! (. mx -e) 5)
        (set! (. mx -f) 6)
        (expect type-eq js/SVGMatrix (op/matrix mx))
        (expect eq mx (op/matrix mx)))))

  (describe "vec<-mx"
    (should "convert an SVGMatrix to a vector of numbers"
      (let [mx (. op/svg-root createSVGMatrix)]
        (set! (. mx -a) 1)
        (set! (. mx -b) 2)
        (set! (. mx -c) 3)
        (set! (. mx -d) 4)
        (set! (. mx -e) 5)
        (set! (. mx -f) 6)
        (expect eq [1 2 3 4 5 6] (op/vec<-mx mx))))
    (should "return non SVGRect arg unchanged"
      (expect eq [1 2 3 4 5 6] (op/vec<-mx [1 2 3 4 5 6]))
      (expect eq :not-a-point (op/vec<-mx :not-a-point))))

  (describe "elem-mx"
    (should "return an SVG element's transform matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (. rec setAttribute "transform" "matrix(1, 2, 3, 4, 5, 6)")
        (expect type-eq js/SVGMatrix (op/elem-mx rec))
        (expect eq [1 2 3 4 5 6] (op/vec<-mx (op/elem-mx rec)))))
    (should "return an identity matrix if element has none"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (expect type-eq js/SVGMatrix (op/elem-mx rec))
        (expect eq [1 0 0 1 0 0] (op/vec<-mx (op/elem-mx rec))))))

  (describe "mx-components"
    (should "return a map of matrix components"
      (let [mx (. op/svg-root createSVGMatrix)
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
          (op/mx-components mx))
        (expect eq {:translation [10 -20]
                    :rotation (. js/Math -PI)
                    :scale [4 0.5]}
          (op/mx-components rec)))))

  (describe "set-elem-mx!"
    (should "set an SVG element's transform matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (op/set-elem-mx! rec [1 2 3 4 5 6])
        (expect eq [1 2 3 4 5 6]
          (op/vec<-mx
            (.. rec -transform -baseVal (consolidate) -matrix)))))
    (should "return the set matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (expect eq (op/vec<-mx (op/set-elem-mx! rec [1 2 3 4 5 6]))
          (op/vec<-mx
            (.. rec -transform -baseVal (consolidate) -matrix))))))

  (describe "elem*mx!"
    (should "multiply an element's matrix by another matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (. rec setAttribute "transform" "matrix(-4, 0, 0, 0.5, 10, -20)")
        (op/elem*mx! rec [1 0 0 1 2 0.5])
        (expect eq [-4, 0, 0, 0.5, 2, -19.75]
          (op/vec<-mx
            (.. rec -transform -baseVal (consolidate) -matrix)))))
    (should "return the updated matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (. rec setAttribute "transform" "matrix(-4, 0, 0, 0.5, 10, -20)")
        (expect eq (op/vec<-mx (op/elem*mx! rec [1 0 0 1 2 0.5]))
          (op/vec<-mx
            (.. rec -transform -baseVal (consolidate) -matrix))))))

  (describe "mx*elem!"
    (should "multiply another matrix by an element's matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (. rec setAttribute "transform" "matrix(-4, 0, 0, 0.5, 10, -20)")
        (op/mx*elem! [1 0 0 1 2 0.5] rec)
        (expect eq [-4, 0, 0, 0.5, 12, -19.5]
          (op/vec<-mx
            (.. rec -transform -baseVal (consolidate) -matrix)))))
    (should "return the updated matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (. rec setAttribute "transform" "matrix(-4, 0, 0, 0.5, 10, -20)")
        (expect eq (op/vec<-mx (op/mx*elem! [1 0 0 1 2 0.5] rec))
          (op/vec<-mx
            (.. rec -transform -baseVal (consolidate) -matrix))))))

  (describe "clear-elem-mx!"
    :let [rec #(let [rec (. js/document createElementNS ns/svgns "rect")]
                 (. rec setAttribute "transform" "matrix(1, 2, 3, 4, 5, 6)")
                 rec)]
    (should "set elem's transform matrix to the identity matrix"
      (let [rec (rec)]
        (op/clear-elem-mx! rec)
        (expect eq [1 0 0 1 0 0]
          (op/vec<-mx
            (.. rec -transform -baseVal (consolidate) -matrix)))))
    (should "return the new identity matrix"
      (let [rec (rec)]
        (expect eq (op/vec<-mx (op/clear-elem-mx! rec))
          (op/vec<-mx
            (.. rec -transform -baseVal (consolidate) -matrix))))))

  (describe "transform-point"
    (should "transform a point by a matrix"
      (expect eq [6 10]
        (op/pair<-point
          (op/transform-point [3 2] (op/matrix 1 0 0 1 3 8))))))

  (describe "transform-rect"
    (should "transform a rect by a matrix"
      (expect eq [[5 4] [8 4] [16 12] [13 12]]
        (map op/pair<-point
             (op/vec<-rect
               (op/transform-rect [1 2 3 4] (op/matrix 1 0 2 2 0 0)))))))

  (describe "with-g-wrap"
    :let [a (. js/document createElementNS ns/svgns "g")
          b (. js/document createElementNS ns/svgns "rect")]
    :before (. a appendChild b)
    (should "wrap an SVG element in a group for the duration of f"
      (expect eq "g" (op/with-g-wrap b (fn [g] (. g -tagName))))
      (expect eq b (op/with-g-wrap b (fn [g] (. g -firstElementChild))))))

  (describe "rects-intersect?"
    (should "return true iff two rects intersect"
      (expect truthy (not (op/rects-intersect? [0 0 1 1] [1 1 1 1])))
      (expect truthy (op/rects-intersect? [0 0 9 9] [8 8 100 100]))
      (expect truthy (op/rects-intersect? [0 0 9 9] [-8 -8 9 9]))
      (expect truthy (op/rects-intersect? [0 0 9 9] [8 -8 100 9]))
      (expect truthy (op/rects-intersect? [0 0 9 9] [-8 8 9 100])))))

;;. vim: set lispwords+=defsuite,describe,should,expect:
