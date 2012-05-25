(ns domicile.css
  (:use
    [clojure.string :only (join)]))

(defn- wrap
  "Wrap `s` in `n` vectors."
  [n s]
  (loop [n n acc s]
    (if (= 0 n)
      acc
      (recur (dec n) [acc]))))

(defn- normalize-selector
  "Normalize selector to the form `[[a b] [c d]]`, which represents the CSS
  selector: 'a b, c d'."
  ([s n]
  (if (vector? s)
    (map #(normalize-selector % (dec n))  s)
    (wrap n (name s))))
  ([s]
   (normalize-selector s 2)))

(defn rules
  "Convert args to CSS rules. See `css` for details."
  [& args]
  (join ";" (for [[k v] (partition 2 args)]
              (str (name k) ":" v))))

(defn- css1
  [[selector & rule-list :as rules*]]
  (str
    (join "," (map #(join " " %)
                   (normalize-selector selector)))
    "{"
    (apply rules rule-list)
    "}"))

(defn css
  "Convert the input to CSS. Input is one or more of the following vectors,
  which represent a single selector-rules pairing:

  `[:selector
    :border \"1px solid red\"
    :border-top \"green\"]`

  Rule order is preserved so later rules can override earlier ones.

  Property names are passed to `name` and values to `str`.

  Selectors are normalized to the form `[[a b] [c d]]`, which represents the
  CSS selector, 'a b, c d'. For examnple,

  :.entity => [[\".entity\"]] => \".entity\"

  [:.entity :.name] => [[\".entity\"] [\".name\"]] => \".entity, .name\"

  [:.entity [:.name :> :.nick]] => [[\".entity\"] [\".name\" \">\" \".nick\"]]
      => \".entity, .name > .nick\"."
  [& decls]
  (join "" (map css1 decls)))
