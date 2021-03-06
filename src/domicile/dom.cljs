(ns domicile.dom
  (:refer-clojure :exclude [ancestors])
  (:require
    [domicile.ns :as ns]
    [domicile.extended-types :as _et]
    [clojure.string :as str]
    [goog :as goog]
    [goog.dom.ViewportSizeMonitor :as viewport])
  (:use
    [clojure.set :only [map-invert]]))

(defn classes
  [elem]
  (let [class (. elem getAttribute "class")]
    (if class
      (disj (set (.. class (trim) (split #"\s+")))
            "")
      #{})))

(defn add-classes!
  [elem class & cs]
  (. elem setAttribute
     "class"
     (str/join " " (apply conj (classes elem) class cs))))

(defn del-classes!
  [elem class & cs]
  (. elem setAttribute
     "class"
     (str/join " " (apply disj (classes elem) class cs))))

(defn remove-node!
  [node]
  (when-let [parent ( . node -parentNode)]
    (. parent removeChild node)))

(defn ancestors
  [node]
  (lazy-seq
    (when node
      (cons node (ancestors (. node -parentNode))))))

(defn ancestors-to
  [node ancestor]
  (let [[up-to after] (split-with (partial not= ancestor) (ancestors node))]
    (if (= ancestor (first after))
      up-to
      nil)))

(defn ancestor?
  [node1 node2]
  (some #{node2} (ancestors node1)))

(defn viewport-size
  []
  (let [size (. (viewport/getInstanceForWindow) getSize)]
    [(. size -width) (. size -height)]))

(defn show-elem!
  [elem]
  (.. elem -style (removeProperty "visibility")))

(defn hide-elem!
  [elem]
  (.. elem -style (setProperty "visibility" "hidden")))

(defn computed-dimensions
  [elem]
  (let [cs (. js/window getComputedStyle elem)]
    [(js/parseInt (. cs getPropertyValue "width"))
     (js/parseInt (. cs getPropertyValue "height"))]))
