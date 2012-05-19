(ns domicile.create-test
  (:require
    [domicile.core :as dom]
    [domicile.create :as create]
    [menodora.core :as mc])
  (:use
    [menodora.predicates :only [eq type-eq]])
  (:use-macros
    [menodora :only [defsuite describe should expect]]))

(defsuite create-tests
  (describe "text"
    (should "create a Text DOM node"
      (expect type-eq js/Text (create/text ""))
      (expect eq "hello" (. (create/text "hello") -textContent))))

  (describe "elem"
    (should "create a DOM Element"
      (expect type-eq js/HTMLDivElement (create/elem "div" {})))
    (should "create a namespaced DOM Element"
      (expect type-eq js/SVGRectElement (create/elem [dom/svgns "rect"] {})))
    (should "add attributes"
      (let [elem (create/elem "div" {:id "foo" [dom/xlinkns :href] "#here"})]
        (expect eq "foo" (. elem getAttribute "id"))
        (expect eq "#here" (. elem getAttributeNS dom/xlinkns "href"))))
    (should "append children"
      (let [elem (create/elem "p" {} (create/elem "b" {} "hello world") " again")]
        (expect type-eq js/HTMLElement (. elem -firstChild))
        (expect type-eq js/Text (. elem -lastChild))
        (expect eq "hello world" (.. elem -firstChild -textContent))
        (expect eq " again" (.. elem -lastChild -textContent))
        (expect eq "hello world again" (. elem -textContent))
        (expect eq "<p><b>hello world</b> again</p>" (. elem -outerHTML))))))

;;. vim: set lispwords+=defsuite,describe,should,expect:
