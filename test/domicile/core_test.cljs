(ns domicile.core-test
  (:require
    [domicile.core :as dom]
    [domicile.svg :as svg]
    [domicile.ns :as ns]
    [menodora.core :as mc])
  (:use
    [clojure.set :only [difference]]
    [menodora.predicates :only [eq type-eq truthy]])
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

  (describe "Element"
    (should "implement ILookup"
      (let [elem (. js/document createElement "div")]
        (. elem setAttribute "id" "abc")
        (expect eq "abc" (:id elem))
        (expect eq nil (:class elem))
        (expect eq "abc" (get elem :id :not-found))
        (expect eq :not-found (get elem :title :not-found))

        (. elem setAttributeNS ns/xlinkns "xlink:href" "#here")
        (expect eq "#here" (get elem [ns/xlinkns :href]))
        (expect eq "#here" (get elem [:xlink :href]))
        (expect eq "#here" (get elem [:xlink "href"]))
        (expect eq "#here" (get elem [ns/xlinkns "href"]))
        (expect eq "#here" (:xlink:href elem))))
    (should "implement ITransientCollection"
      (let [elem (. js/document createElement "div")]
        (expect eq elem (conj! elem [:id "abc"]))
        (expect eq "abc" (. elem getAttribute "id"))
        (expect eq elem (conj! elem {:id "def" [ns/xlinkns :href] "#here"}))
        (expect eq "def" (. elem getAttribute "id"))
        (expect eq "#here" (. elem getAttributeNS ns/xlinkns "href"))
        (dorun
          (for [[k n] [[[ns/xlinkns :href] 1]
                       [[:xlink :href] 2]
                       [[:xlink "href"] 3]
                       [[ns/xlinkns "href"] 4]
                       [:xlink:href 5]]
                :let [id (str "#here" n)]]
            (do
              (expect eq elem (conj! elem [k id]))
              (expect eq id (. elem getAttributeNS ns/xlinkns "href")))))

        (expect eq {:id "def" :xlink:href "#here5"} (persistent! elem))))
    (should "implement ITransientAssociative"
      (let [elem (. js/document createElement "div")]
        (expect eq elem (assoc! elem :id "abc"))
        (expect eq "abc" (. elem getAttribute "id"))
        (assoc! elem :id "def")
        (expect eq "def" (. elem getAttribute "id"))

        (expect eq elem (assoc! elem [ns/xlinkns :href] "#here"))
        (expect eq "#here" (. elem getAttributeNS ns/xlinkns "href"))))
    (should "implement ITransientMap"
      (let [elem (. js/document createElement "div")]
        (. elem setAttribute "id" "abc")
        (expect eq elem (dissoc! elem :id))
        (expect eq nil (. elem getAttribute "id"))
        (dissoc! elem :id)
        (expect eq nil (. elem getAttribute "id"))

        (. elem setAttributeNS ns/xlinkns "href" "#here")
        (expect eq elem (dissoc! elem [ns/xlinkns :href]))
        (expect eq "" (. elem getAttributeNS ns/xlinkns "href")))))

  (describe "CSSStyleDeclaration"
    (should "implement ILookup"
      (let [node (. js/document createElement "div")
            css (. node -style)]
        (set! (. css -color) "red")
        (expect eq "red" (:color css))
        (expect eq nil (:border css))
        (expect eq "red" (get css :color :not-found))
        (expect eq :not-found (get css :background :not-found))))
    (should "implement ITransientCollection"
      (let [node (. js/document createElement "div")
            css (. node -style)]
        (expect eq css (conj! css [:color "red"]))
        (expect eq "red" (. css -color))
        (expect eq css (conj! css {:color "blue" :background "yellow"}))
        (expect eq "blue" (. css -color))
        (expect eq "yellow" (. css -background))

        (expect eq #{} (difference (set {:color "blue" :background "yellow"})
                                   (set (persistent! css))))))
    (should "implement ITransientAssociative"
      (let [node (. js/document createElement "div")
            css (. node -style)]
        (expect eq css (assoc! css :color "red"))
        (expect eq "red" (. css -color))
        (assoc! css :color "blue")
        (expect eq "blue" (. css -color))))
    (should "implement ITransientMap"
      (let [node (. js/document createElement "div")
            css (. node -style)]
        (set! (. css -color) "red")
        (expect eq css (dissoc! css :color))
        (expect eq "" (. css -color))
        (dissoc! css :color)
        (expect eq "" (. css -color)))))

  (describe "computed styles"
    (should "implement ILookup"
      (let [node (. js/document -body)
            css (. js/window getComputedStyle node)]
        (set! (.. node -style -color) "red")
        (expect eq "rgb(255, 0, 0)" (:color css))
        (expect eq "0px none rgb(255, 0, 0)" (:border css))
        (expect eq "rgb(255, 0, 0)" (get css :color :not-found))))
    (should "implement ITransientCollection"
      (let [node (. js/document -body)
            css (. js/window getComputedStyle node)]
        (expect eq :error (try (conj! css [:color "red"])
                            (catch js/Object e :error)))

        (expect type-eq PersistentHashMap (persistent! css))))
    (should "implement ITransientAssociative"
      (let [node (. js/document createElement "div")
            css (. js/window getComputedStyle node)]
        (expect eq :error (try (assoc! css :color "red")
                            (catch js/Object e :error)))))
    (should "implement ITransientMap"
      (let [node (. js/document createElement "div")
            css (. js/window getComputedStyle node)]
        (expect eq :error (try (dissoc! css :color)
                            (catch js/Object e :error))))))

  (describe "Props"
    (should "implement ILookup"
      (let [node (. js/document createElement "div")
            props (dom/props node)]
        (set! (. node -foo) "abc")
        (expect eq "abc" (:foo props))
        (expect eq nil (:bar props))
        (expect eq "abc" (get props :foo :not-found))
        (expect eq :not-found (get props :bar :not-found))))
    (should "implement ITransientCollection"
      (let [node (. js/document createElement "div")
            props (dom/props node)]
        (expect eq props (conj! props [:innerText "hello"]))
        (expect eq "hello" (. node -innerText))
        (expect eq props (conj! props {:innerText "world" :title "greeting"}))
        (expect eq "world" (. node -innerText))
        (expect eq "greeting" (. node -title))))
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

  (describe "SvgProps"
    (should "implement ILookup"
      ;; prop -baseVal -value
      (let [node (. js/document createElementNS ns/svgns "rect")
            props (svg/props node)]
        (set! (.. node -x -baseVal -value) 10)
        (expect eq 10 (:x props))
        (expect eq nil (:bar props))
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
        (expect eq 11 (.. node -x -baseVal -value))))))

;;. vim: set lispwords+=defsuite,describe,should,expect:
