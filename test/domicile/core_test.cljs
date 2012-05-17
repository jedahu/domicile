(ns domicile.core-test
  (:require
    [domicile.core :as dom]
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
      (expect type-eq dom/DomList classes)
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
        (assoc! attrs :id "abc")
        (expect eq "abc" (. node getAttribute "id"))
        (assoc! attrs :id "def")
        (expect eq "def" (. node getAttribute "id"))))
    (should "implement ITransientMap"
      (let [node (. js/document createElement "div")
            attrs (dom/attrs node)]
        (. node setAttribute "id" "abc")
        (dissoc! attrs :id)
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
        (assoc! css :color "red")
        (expect eq "red" (.. node -style -color))
        (assoc! css :color "blue")
        (expect eq "blue" (.. node -style -color))))
    (should "implement ITransientMap"
      (let [node (. js/document createElement "div")
            css (dom/css node)]
        (set! (.. node -style -color) "red")
        (dissoc! css :color)
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
        (assoc! props :foo "abc")
        (expect eq "abc" (. node -foo))
        (assoc! props :foo "def")
        (expect eq "def" (. node -foo))))
    (should "implement ITransientMap"
      (let [node (. js/document createElement "div")
            props (dom/props node)]
        (set! (. node -foo) "abc")
        (dissoc! props :foo)
        (expect eq nil (. node -foo))
        (dissoc! props :foo)
        (expect eq nil (. node -foo))))
    (should "wrap dom lists"
      (let [node (. js/document createElement "div")
            props (dom/props node)]
        (. node setAttribute "class" "one two three")
        (expect type-eq dom/DomList (:classList props))))))

;;. vim: set lispwords+=defsuite,describe,should,expect:
