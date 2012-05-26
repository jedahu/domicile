(ns domicile.svg.extended-types-test
  (:require
    [domicile.svg :as svg]
    [domicile.ns :as ns]
    [menodora.core :as mc])
  (:use
    [menodora.predicates :only [eq type-eq truthy]])
  (:use-macros
    [menodora :only [defsuite describe should expect]]))

(defsuite svg-extended-types-tests
  (describe "SVGPoint"
    (should "destructure"
      (let [pt (. svg/svg-root createSVGPoint)]
        (set! (. pt -x) 1)
        (set! (. pt -y) 2)
        (let [[x y] pt]
          (expect eq [1 2] [x y]))
        (let [{:keys [x y]} pt]
          (expect eq [1 2] [x y])))))

  (describe "SVGRect"
    (should "destructure"
      (let [rec (. svg/svg-root createSVGRect)]
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
      (let [mx (. svg/svg-root createSVGMatrix)]
        (set! (. mx -a) 1)
        (set! (. mx -b) 2)
        (set! (. mx -c) 3)
        (set! (. mx -d) 4)
        (set! (. mx -e) 5)
        (set! (. mx -f) 6)
        (let [[a b c d e f] mx]
          (expect eq [1 2 3 4 5 6] [a b c d e f]))
        (let [{:keys [a b c d e f]} mx]
          (expect eq [1 2 3 4 5 6] [a b c d e f])))))

  (describe "SVGTransform"
    (should "destructure"
      (let [tr (. svg/svg-root createSVGTransform)]
        (. tr setTranslate 3 4)
        (let [[type x y] tr]
          (expect eq [:translate 3 4] [type x y]))
        (. tr setRotate 90 3 8)
        (let [[type a x y] tr]
          (expect eq [:rotate 90 11 5] [type a x y]))
        (. tr setScale 3 4)
        (let [[type x y] tr]
          (expect eq [:scale 3 4] [type x y])))))

  (describe "SVGPathSeg"
    (should "destructure"
      (let [seg (. svg/path-elem createSVGPathSegArcAbs 1 2 3 4 5 false true)]
        (let [[k x y r1 r2 angle largeArcFlag sweepFlag] seg]
          (expect eq [:A 1 2 3 4 5 false true] [k x y r1 r2 angle largeArcFlag sweepFlag]))
        (let [{:keys [type x y r1 r2 angle largeArcFlag sweepFlag]} seg]
          (expect eq [:A 1 2 3 4 5 false true] [type x y r1 r2 angle largeArcFlag sweepFlag]))))))

;;. vim: set lispwords+=defsuite,describe,should,expect:
