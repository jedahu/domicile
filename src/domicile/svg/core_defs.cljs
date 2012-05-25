(ns domicile.svg.core-defs
  (:require
    [domicile.ns :as ns])
  (:use-macros
    [domicile.util.macros :only [japply]]))

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
