(ns domicile.core-test
  (:require
    [domicile.core :as dom]
    [menodora.core :as mc])
  (:use
    [menodora.predicates :only [eq]])
  (:use-macros
    [menodora :only [defsuite describe should expect]]))

(defsuite core-tests
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
        (expect eq :not-found (get css :background :not-found))))))

;;. vim: set lispwords+=defsuite,describe,should,expect:
