(ns domicile.create-test
  (:require
    [domicile.core :as dom]
    [domicile.ns :as ns]
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
      (expect type-eq js/SVGRectElement (create/elem [ns/svgns "rect"] {})))
    (should "add attributes"
      (let [elem (create/elem "div" {:id "foo" [ns/xlinkns :href] "#here"})]
        (expect eq "foo" (. elem getAttribute "id"))
        (expect eq "#here" (. elem getAttributeNS ns/xlinkns "href"))))
    (should "append children"
      (let [elem (create/elem "p" {} (create/elem "b" {} "hello world") " again")]
        (expect type-eq js/HTMLElement (. elem -firstChild))
        (expect type-eq js/Text (. elem -lastChild))
        (expect eq "hello world" (.. elem -firstChild -textContent))
        (expect eq " again" (.. elem -lastChild -textContent))
        (expect eq "hello world again" (. elem -textContent))
        (expect eq "<p><b>hello world</b> again</p>" (. elem -outerHTML)))))

  (describe "html-document"
    (should "create a HTML document"
      (expect type-eq js/HTMLDocument (create/html-document "title")))
    (should "set title"
      (expect eq "abc" (. (create/html-document "abc") -title)))
    (should "append head and body"
      (let [doc (create/html-document "abc"
                                      [(create/elem "script" {:src "foo.js"})
                                       (create/elem "script" {} "alert('boo!')")]
                                      (create/elem "h1" {} "Hello World")
                                      (create/elem "p" {:class "intro"} "blah"))
            scripts (dom/dom-list (. doc getElementsByTagName "script"))
            h1 (.. doc (getElementsByTagName "h1") (item 0))
            p (.. doc (getElementsByTagName "p") (item 0))]
        (expect type-eq js/HTMLDocument doc)
        (expect eq "abc" (. doc -title))
        (expect eq "foo.js" (. (first scripts) getAttribute "src"))
        (expect eq "alert('boo!')" (. (second scripts) -textContent))
        (expect eq "Hello World" (. h1 -textContent))
        (expect eq "blah" (. p -textContent))))))


;;. vim: set lispwords+=defsuite,describe,should,expect:
