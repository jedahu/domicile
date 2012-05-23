(ns domicile.svg.create
  (:require
    [domicile.ns :as ns]))

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
   (apply point xy)))

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
   (apply rect xywh)))

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
   (apply matrix abcdef)))
