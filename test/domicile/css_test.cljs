(ns domicile.css-test
  (:require
    [domicile.css :as css]
    [menodora.core :as mc])
  (:use
    [menodora.predicates :only [eq type-eq truthy]])
  (:use-macros
    [menodora :only [defsuite describe should expect]]))

(defsuite css-tests
  (describe "rules"
    (should "return a CSS string"
      (expect eq "color:red;border:1px solid green"
        (css/rules :color "red" :border "1px solid green"))
      (expect eq "content:'abc'" (css/rules :content "'abc'"))
      (expect eq "opacity:0.3" (css/rules :opacity 0.3))))

  (describe "css"
    (should "return a CSS string"
      (expect eq (str "div{color:red;border:1px solid green}"
                      "span{opacity:0.3}")
        (css/css [:div :color "red" :border "1px solid green"]
                 [:span :opacity 0.3])))
    (should "normalize selectors"
      (expect eq "div,span{}" (css/css [[:div :span]]))
      (expect eq "div,span{}" (css/css [[[:div] [:span]]]))
      (expect eq "div span{}" (css/css [[[:div :span]]]))
      (expect eq "a > b,c,d f{}" (css/css [[[:a :> :b] :c [:d :f]]])))))

;;. vim: set lispwords+=defsuite,describe,should,expect:
