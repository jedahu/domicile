(ns domicile.core-test
  (:require
    [domicile.core :as dom]
    [domicile.svg :as svg]
    [menodora.core :as mc])
  (:use
    [menodora.predicates :only [eq type-eq]])
  (:use-macros
    [menodora :only [defsuite describe should expect]]))

(defsuite core-tests
  (describe "DomList"
    :let [classes (let [node (. js/document createElement "div")]
                    (. node setAttribute "class" "one two three")
                    (dom/dom-list (. node -classList)))]
    (should "implement ISeqable"
      (expect eq (seq ["one" "two" "three"]) (seq classes)))
    (should "implement ICounted"
      (expect eq 3 (count classes)))
    (should "implement IReduce"
      (expect eq "onetwothree" (reduce str classes))
      (expect eq "zeroonetwothree" (reduce str "zero" classes)))
    (should "implememnt IIndexed"
      (expect eq "two" (nth classes 1))
      (expect eq "two" (nth classes 1 :not-found))
      (expect eq :not-found (nth classes 3 :not-found))))

  (describe "Attrs"
    (should "implement ILookup"
      (let [node (. js/document createElement "div")
            attrs (dom/attrs node)]
        (. node setAttribute "id" "abc")
        (expect eq "abc" (:id attrs))
        (expect eq nil (:class attrs))
        (expect eq "abc" (get attrs :id :not-found))
        (expect eq :not-found (get attrs :title :not-found))))
    (should "implement ITransientAssociative"
      (let [node (. js/document createElement "div")
            attrs (dom/attrs node)]
        (expect eq attrs (assoc! attrs :id "abc"))
        (expect eq "abc" (. node getAttribute "id"))
        (assoc! attrs :id "def")
        (expect eq "def" (. node getAttribute "id"))))
    (should "implement ITransientMap"
      (let [node (. js/document createElement "div")
            attrs (dom/attrs node)]
        (. node setAttribute "id" "abc")
        (expect eq attrs (dissoc! attrs :id))
        (expect eq nil (. node getAttribute "id"))
        (dissoc! attrs :id)
        (expect eq nil (. node getAttribute "id")))))

  (describe "Styles"
    (should "implement ILookup"
      (let [node (. js/document createElement "div")
            css (dom/css node)]
        (set! (.. node -style -color) "red")
        (expect eq "red" (:color css))
        (expect eq nil (:border css))
        (expect eq "red" (get css :color :not-found))
        (expect eq :not-found (get css :background :not-found))))
    (should "implement ITransientAssociative"
      (let [node (. js/document createElement "div")
            css (dom/css node)]
        (expect eq css (assoc! css :color "red"))
        (expect eq "red" (.. node -style -color))
        (assoc! css :color "blue")
        (expect eq "blue" (.. node -style -color))))
    (should "implement ITransientMap"
      (let [node (. js/document createElement "div")
            css (dom/css node)]
        (set! (.. node -style -color) "red")
        (expect eq css (dissoc! css :color))
        (expect eq "" (.. node -style -color))
        (dissoc! css :color)
        (expect eq "" (.. node -style -color)))))

  (describe "Props"
    (should "implement ILookup"
      (let [node (. js/document createElement "div")
            props (dom/props node)]
        (set! (. node -foo) "abc")
        (expect eq "abc" (:foo props))
        (expect eq nil (:bar props))
        (expect eq "abc" (get props :foo :not-found))
        (expect eq :not-found (get props :bar :not-found))))
    (should "implement ITransientAssociative"
      (let [node (. js/document createElement "div")
            props (dom/props node)]
        (expect eq props (assoc! props :foo "abc"))
        (expect eq "abc" (. node -foo))
        (assoc! props :foo "def")
        (expect eq "def" (. node -foo))))
    (should "implement ITransientMap"
      (let [node (. js/document createElement "div")
            props (dom/props node)]
        (set! (. node -foo) "abc")
        (expect eq props (dissoc! props :foo))
        (expect eq nil (. node -foo))
        (dissoc! props :foo)
        (expect eq nil (. node -foo))))
    (should "wrap dom lists"
      (let [node (. js/document createElement "div")
            props (dom/props node)]
        (. node setAttribute "class" "one two three")
        (expect type-eq dom/DomList (:classList props)))))

  (describe "SvgList"
    :let [points #(let [node (. js/document createElementNS dom/svgns "polygon")]
                    (. node setAttribute "points" "0 0 100 100 20 80")
                    (svg/svg-list (.. node -points)))
          root (. js/document createElementNS dom/svgns "svg")
          point (let [pt (. root createSVGPoint)]
                  (set! (. pt -x) 3)
                  (set! (. pt -y) 9)
                  pt)]
    (should "implement ISeqable"
      (expect type-eq svg/SvgList (points))
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
        (expect eq point (. (dom/underlying pts) getItem 3))
        (expect type-eq PersistentVector (persistent! pts))
        (expect eq [[0 0] [100 100] [20 80] [3 9]]
          (vec (map (fn [p] [(. p -x) (. p -y)]) (persistent! pts))))))
    (should "implement ITransientAssociative"
      (let [pts (points)]
        (expect eq pts (assoc! pts point 0))
        (expect eq point (. (dom/underlying pts) getItem 0))))
    (should "implement ITransientVector"
      (let [pts (points)]
        (expect eq pts (pop! pts))
        (expect eq 2 (count pts)))))

  (describe "SvgProps"
    (should "implement ILookup"
      ;; prop -baseVal -value
      (let [node (. js/document createElementNS dom/svgns "rect")
            props (svg/props node)]
        (set! (.. node -x -baseVal -value) 10)
        (expect eq 10 (:x props))
        (expect eq nil (:bar props))
        (expect eq 10 (get props :x :not-found))
        (expect eq :not-found (get props :bar :not-found)))
      ;; prop -baseVal
      (let [node (. js/document createElementNS dom/svgns "path")
            props (svg/props node)]
        (. node setAttribute "pathLength" "3")
        (expect eq 3 (:pathLength props)))
      ;; prop -baseVal (list)
      (let [node (. js/document createElementNS dom/svgns "polyline")
            props (svg/props node)]
        (. node setAttribute "points" "0 0 100 100 20 80")
        (expect type-eq svg/SvgList (:points props))
        (expect eq 3 (count (:points props)))))
    (should "implement ITransientAssociative"
      (let [node (. js/document createElementNS dom/svgns "rect")
            props (svg/props node)]
        (expect eq props (assoc! props :x 10))
        (expect eq 10 (.. node -x -baseVal -value))
        (assoc! props :x 11)
        (expect eq 11 (.. node -x -baseVal -value))))
    (should "wrap svg lists"
      (let [node (. js/document createElementNS dom/svgns "polygon")
            props (svg/props node)]
        (. node setAttribute "points" "0 0 100 100 20 80")
        (expect type-eq svg/SvgList (:points props))))))

;;. vim: set lispwords+=defsuite,describe,should,expect:
