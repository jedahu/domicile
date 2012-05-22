(ns domicile.operate-test
  (:require
    [domicile.operate :as op]
    [menodora.core :as mc])
  (:use
    [menodora.predicates :only [eq type-eq truthy]])
  (:use-macros
    [menodora :only [defsuite describe should expect]]))

(defsuite operate-tests
  (describe "classes"
    :let [elem (fn [classes]
                 (let [e (. js/document createElement "div")]
                   (. e setAttribute "class" classes)
                   e))]
    (should "return a set"
      (expect eq #{"a"} (op/classes (elem "a")))
      (expect eq #{"a" "b"} (op/classes (elem "a b")))
      (expect eq #{"a" "b"} (op/classes (elem "a b a"))))
    (should "ignore whitespace"
      (expect eq #{"a"} (op/classes (elem " a ")))
      (expect eq #{"a" "b"} (op/classes (elem " a   b "))))
    (should "return the empty set if no classes"
      (expect eq #{} (op/classes (. js/document createElement "div")))
      (let [elem (. js/document createElement "div")]
        (. elem setAttribute "class" "")
        (expect eq #{} (op/classes elem)))))

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
      (expect eq [d c b a] (vec (op/ancestors d)))
      (expect eq [a] (vec (op/ancestors a)))))

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
      (expect eq [d c b] (vec (op/ancestors-to d a))))
    (should "return nil if ancestor is not an ancestor"
      (expect eq nil (op/ancestors-to b d)))
    (should "return empty sequence if node is ancestor"
      (expect eq () (op/ancestors-to d d))))

  (describe "viewport-size"
    (should "return a pair of numbers"
      (expect truthy (vector? (op/viewport-size)))
      (expect truthy (number? (first (op/viewport-size))))
      (expect truthy (number? (second (op/viewport-size)))))))

;;. vim: set lispwords+=defsuite,describe,should,expect:
