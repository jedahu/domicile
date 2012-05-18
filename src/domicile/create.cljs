(ns domicile.create
  (:require
    [domicіle.core :as dom]))

(defn text
  [s]
  (. js/document createTextNode s))

(defn node
  [tag attrs & children]
  (let [dom-node (if-let [[ns tag] (and (vector? tag) tag)]
                   (. js/document createElementNS ns tag)
                   (. js/document createElement tag))]
    (merge (dom/attrs dom-node) attrs)
    (doseq [c children]
      (. dom-node appendChild
         (if (string? c)
           (text c)
           c)))
    dom-node))
