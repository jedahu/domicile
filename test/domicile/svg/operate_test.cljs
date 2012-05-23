(ns domicile.svg.operate-test
  (:require
    [domicile.svg.operate :as op]
    [menodora.core :as mc])
  (:use
    [menodora.predicates :only [eq type-eq]])
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
        (expect eq mx (op/matrix mx))))))

;;. vim: set lispwords+=defsuite,describe,should,expect:
