(ns domicile.create
  (:require
    [domicile.ns :as ns]
    [domicile.extended-types :as _et]))

(def ^:dynamic *document* nil)

(defn get-document
  []
  (or *document* js/document))

(defn text
  [s]
  (. (get-document) createTextNode s))

(defn elem
  [tag attrs & children]
  (let [dom-node (let [[ns local] (ns/normalize-name tag)]
                   (if ns
                     (. (get-document) createElementNS ns local)
                     (. (get-document) createElement local)))]
    (conj! dom-node attrs)
    (doseq [c children]
      (. dom-node appendChild
         (if (string? c)
           (text c)
           c)))
    dom-node))

(defn document
  [root & children]
  (let [[ns local] (ns/normalize-name root)
        doc (.. (get-document) -implementation (createDocument ns local nil))
        doc-root (. doc -documentElement)]
    (doseq [c children]
      (. doc-root appendChild c))
    doc))

(defn html-document
  [title & [head & body]]
  (let [doc (.. (get-document) -implementation (createHTMLDocument title))
        doc-head (. doc -head)
        doc-body (. doc -body)]
    (doseq [h head]
      (. doc-head appendChild h))
    (doseq [b body]
      (. doc-body appendChild b))
    doc))
