(ns domicile.operate
  (:refer-clojure :exclude [ancestors])
  (:require
    [domicile.core :as dom]
    [goog.dom.ViewportSizeMonitor :as viewport]))

(defn classes
  [elem]
  (let [class (. elem getAttribute "class")]
    (if class
      (disj (set (.. class (trim) (split #"\s+")))
            "")
      #{})))

(defn remove-node
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
