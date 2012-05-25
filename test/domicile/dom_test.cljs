(ns domicile.dom-test
  (:require
    [domicile.dom :as dom]
    [menodora.core :as mc])
  (:use
    [menodora.predicates :only [eq type-eq truthy]])
  (:use-macros
    [menodora :only [defsuite describe should expect]]))

(defsuite dom-tests
  (describe "classes"
    :let [elem (fn [classes]
                 (let [e (. js/document createElement "div")]
                   (. e setAttribute "class" classes)
                   e))]
    (should "return a set"
      (expect eq #{"a"} (dom/classes (elem "a")))
      (expect eq #{"a" "b"} (dom/classes (elem "a b")))
      (expect eq #{"a" "b"} (dom/classes (elem "a b a"))))
    (should "ignore whitespace"
      (expect eq #{"a"} (dom/classes (elem " a ")))
      (expect eq #{"a" "b"} (dom/classes (elem " a   b "))))
    (should "return the empty set if no classes"
      (expect eq #{} (dom/classes (. js/document createElement "div")))
      (let [elem (. js/document createElement "div")]
        (. elem setAttribute "class" "")
        (expect eq #{} (dom/classes elem)))))

  (describe "ancestors"
    :let [a (. js/document createElement "div")
          b (. js/document createElement "div")
          c (. js/document createElement "div")
          d (. js/document createElement "div")]
    :before (do
              (. a appendChild b)
              (. b appendChild c)
              (. c appendChild d))
    (should "return correct sequence"
      (expect eq [d c b a] (vec (dom/ancestors d)))
      (expect eq [a] (vec (dom/ancestors a)))))

  (describe "ancestors-to"
    :let [a (. js/document createElement "div")
          b (. js/document createElement "div")
          c (. js/document createElement "div")
          d (. js/document createElement "div")]
    :before (do
              (. a appendChild b)
              (. b appendChild c)
              (. c appendChild d))
    (should "return correct sequence"
      (expect eq [d c b] (vec (dom/ancestors-to d a))))
    (should "return nil if ancestor is not an ancestor"
      (expect eq nil (dom/ancestors-to b d)))
    (should "return empty sequence if node is ancestor"
      (expect eq () (dom/ancestors-to d d))))

  (describe "viewport-size"
    (should "return a pair of numbers"
      (expect truthy (vector? (dom/viewport-size)))
      (expect truthy (number? (first (dom/viewport-size))))
      (expect truthy (number? (second (dom/viewport-size)))))))

;;. vim: set lispwords+=defsuite,describe,should,expect:
