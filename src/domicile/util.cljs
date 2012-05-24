(ns domicile.util)

(defn update!
  [tcoll k f & args]
  (let [v (get tcoll k)]
    (assoc! tcoll k (apply f v args))))
