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

  (describe "elem-mx"
    (should "return an SVG element's transform matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (. rec setAttribute "transform" "matrix(1, 2, 3, 4, 5, 6)")
        (expect type-eq js/SVGMatrix (op/elem-mx rec))
        (expect eq [1 2 3 4 5 6] (vec (op/elem-mx rec)))))
    (should "return an identity matrix if element has none"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (expect type-eq js/SVGMatrix (op/elem-mx rec))
        (expect eq [1 0 0 1 0 0] (vec (op/elem-mx rec))))))

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
          (vec (.. rec -transform -baseVal (consolidate) -matrix)))))
    (should "return the set matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (expect eq (vec (op/set-elem-mx! rec [1 2 3 4 5 6]))
          (vec (.. rec -transform -baseVal (consolidate) -matrix))))))

  (describe "elem*mx!"
    (should "multiply an element's matrix by another matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (. rec setAttribute "transform" "matrix(-4, 0, 0, 0.5, 10, -20)")
        (op/elem*mx! rec [1 0 0 1 2 0.5])
        (expect eq [-4, 0, 0, 0.5, 2, -19.75]
          (vec (.. rec -transform -baseVal (consolidate) -matrix)))))
    (should "return the updated matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (. rec setAttribute "transform" "matrix(-4, 0, 0, 0.5, 10, -20)")
        (expect eq (vec (op/elem*mx! rec [1 0 0 1 2 0.5]))
          (vec (.. rec -transform -baseVal (consolidate) -matrix))))))

  (describe "mx*elem!"
    (should "multiply another matrix by an element's matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (. rec setAttribute "transform" "matrix(-4, 0, 0, 0.5, 10, -20)")
        (op/mx*elem! [1 0 0 1 2 0.5] rec)
        (expect eq [-4, 0, 0, 0.5, 12, -19.5]
          (vec (.. rec -transform -baseVal (consolidate) -matrix)))))
    (should "return the updated matrix"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (. rec setAttribute "transform" "matrix(-4, 0, 0, 0.5, 10, -20)")
        (expect eq (vec (op/mx*elem! [1 0 0 1 2 0.5] rec))
          (vec (.. rec -transform -baseVal (consolidate) -matrix))))))

  (describe "clear-elem-mx!"
    :let [rec #(let [rec (. js/document createElementNS ns/svgns "rect")]
                 (. rec setAttribute "transform" "matrix(1, 2, 3, 4, 5, 6)")
                 rec)]
    (should "set elem's transform matrix to the identity matrix"
      (let [rec (rec)]
        (op/clear-elem-mx! rec)
        (expect eq [1 0 0 1 0 0]
          (vec (.. rec -transform -baseVal (consolidate) -matrix)))))
    (should "return the new identity matrix"
      (let [rec (rec)]
        (expect eq (vec (op/clear-elem-mx! rec))
          (vec (.. rec -transform -baseVal (consolidate) -matrix))))))

  (describe "transform-point"
    (should "transform a point by a matrix"
      (expect eq [6 10]
        (vec (op/transform-point [3 2] (op/matrix 1 0 0 1 3 8))))))

  (describe "transform-rect"
    (should "transform a rect by a matrix"
      (expect eq [[5 4] [8 4] [16 12] [13 12]]
        (map vec (op/transform-rect [1 2 3 4] (op/matrix 1 0 2 2 0 0))))))

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
      (expect truthy (op/rects-intersect? [0 0 9 9] [-8 8 9 100]))))

  (describe "center-origin!"
    :let [test-elem
          (fn [elem]
            (let [svg (. js/document createElementNS ns/svgns "svg")
                  bb1 (. js/document createElementNS ns/svgns "rect")
                  bb2 (. js/document createElementNS ns/svgns "rect")]
              (try
                (.. js/document -body (appendChild svg))
                (. svg appendChild elem)
                (. svg appendChild bb1)
                (. svg appendChild bb2)
                (set! (.. bb1 -x -baseVal -value) (.. elem (getBBox) -x))
                (set! (.. bb1 -y -baseVal -value) (.. elem (getBBox) -y))
                (set! (.. bb1 -width -baseVal -value) (.. elem (getBBox) -width))
                (set! (.. bb1 -height -baseVal -value) (.. elem (getBBox) -height))
                (. bb1 setAttribute "stroke" "red")
                (. bb1 setAttribute "fill" "none")
                (let [[a1 b1 c1 d1 e1 f1 :as mx1] (op/elem-mx elem)
                      [x1 y1 w1 h1 :as bb1] (op/with-g-wrap elem #(. % getBBox))
                      [cx1 cy1] (op/rect-center bb1)
                      [a2 b2 c2 d2 e2 f2 :as mx2] (op/center-origin! elem)
                      [x2 y2 w2 h2 :as bb2] (op/with-g-wrap elem #(. % getBBox))
                      [cx2 cy2] (op/rect-center bb2)
                      [a3 b3 c3 d3 e3 f3 :as mx3] (op/center-origin! elem)
                      [x3 y3 w3 h3 :as bb3] (op/with-g-wrap elem #(. % getBBox))
                      [cx3 cy3] (op/rect-center bb3)]
                  (expect eq [a1 b1 c1 d1] [a2 b2 c2 d2])
                  (expect eq [a2 b2 c2 d2] [a3 b3 c3 d3])
                  (expect eq [a1 b1 c1 d1 (+ e1 cx1) (+ f1 cy1)] (vec mx2))
                  (expect eq (vec bb1) (vec bb2))
                  (expect eq (vec bb2) (vec bb3))
                  (expect eq [cx1 cy1] [cx2 cy2])
                  (expect eq [cx2 cy2] [cx3 cy3])
                  (expect eq [0 0] (op/rect-center (. elem getBBox))))
                (set! (.. bb2 -x -baseVal -value) (.. elem (getBBox) -x))
                (set! (.. bb2 -y -baseVal -value) (.. elem (getBBox) -y))
                (set! (.. bb2 -width -baseVal -value) (.. elem (getBBox) -width))
                (set! (.. bb2 -height -baseVal -value) (.. elem (getBBox) -height))
                (. bb2 setAttribute "stroke" "blue")
                (. bb2 setAttribute "fill" "none")
                (let [oo1 (op/rect-center (op/with-g-wrap elem #(. % getBBox)))]
                  (op/set-elem-mx!
                    elem (.. (op/elem-mx elem)
                           (rotate 90)
                           (scale 2)))
                  (expect eq oo1 (op/rect-center (op/with-g-wrap elem #(. % getBBox))))
                  (op/set-elem-mx!
                    elem (.. (op/elem-mx elem)
                           (rotate -90)
                           (scale 0.5))))
                (catch js/Object e
                  (throw e))
                (finally
                  (.. js/document -body (removeChild svg))))))]
    (should "work for SVGTextElement"
      (let [txt (. js/document createElementNS ns/svgns "text")]
        (.. txt -x -baseVal (appendItem (op/length 60)))
        (.. txt -y -baseVal (appendItem (op/length 60)))
        (. txt setAttribute "font-size" 50)
        (set! (. txt -textContent) "asdf")
        (test-elem txt)))
    (should "work for SVGRectElement"
      (let [rec (. js/document createElementNS ns/svgns "rect")]
        (set! (.. rec -x -baseVal -value) 60)
        (set! (.. rec -y -baseVal -value) 60)
        (set! (.. rec -width -baseVal -value) 70)
        (set! (.. rec -height -baseVal -value) 30)
        (test-elem rec)))
    (should "work for SVGImageElement"
      (let [img (. js/document createElementNS ns/svgns "image")]
        (set! (.. img -x -baseVal -value) 60)
        (set! (.. img -y -baseVal -value) 60)
        (set! (.. img -width -baseVal -value) 70)
        (set! (.. img -height -baseVal -value) 30)
        (test-elem img))))

  (describe "SVGPoint"
    (should "destructure"
      (let [pt (. op/svg-root createSVGPoint)]
        (set! (. pt -x) 1)
        (set! (. pt -y) 2)
        (let [[x y] pt]
          (expect eq [1 2] [x y]))
        (let [{:keys [x y]} pt]
          (expect eq [1 2] [x y])))))

  (describe "SVGRect"
    (should "destructure"
      (let [rec (. op/svg-root createSVGRect)]
        (set! (. rec -x) 1)
        (set! (. rec -y) 2)
        (set! (. rec -width) 3)
        (set! (. rec -height) 4)
        (let [[x y w h] rec]
          (expect eq [1 2 3 4] [x y w h]))
        (let [{:keys [x y w h]} rec]
          (expect eq [1 2 3 4] [x y w h])))))

  (describe "SVGMatrix"
    (should "destructure"
      (let [mx (. op/svg-root createSVGMatrix)]
        (set! (. mx -a) 1)
        (set! (. mx -b) 2)
        (set! (. mx -c) 3)
        (set! (. mx -d) 4)
        (set! (. mx -e) 5)
        (set! (. mx -f) 6)
        (let [[a b c d e f] mx]
          (expect eq [1 2 3 4 5 6] [a b c d e f]))
        (let [{:keys [a b c d e f]} mx]
          (expect eq [1 2 3 4 5 6] [a b c d e f]))))))

;;. vim: set lispwords+=defsuite,describe,should,expect:
