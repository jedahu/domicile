(ns domicile.create
  (:require
    [domicile.core :as dom]))

(defn text
  [s]
  (. js/document createTextNode s))

(defn elem
  [tag attrs & children]
  (let [dom-node (if-let [[ns tag] (and (vector? tag) tag)]
                   (. js/document createElementNS ns tag)
                   (. js/document createElement tag))]
    (dom/merge! (dom/attrs dom-node) attrs)
    (doseq [c children]
      (. dom-node appendChild
         (if (string? c)
           (text c)
           c)))
    dom-node))
