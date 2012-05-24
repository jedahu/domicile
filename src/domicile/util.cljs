(ns domicile.util)

(defn assoc-change!
  [tcoll k f & args]
  (let [v (get tcoll k)]
    (assoc! tcoll k (apply f v args))))
