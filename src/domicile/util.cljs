(ns domicile.util
  (:refer-clojure :exclude [assoc! dissoc!]))

(defn update!
  [tcoll k f & args]
  (let [v (get tcoll k)]
    (assoc! tcoll k (apply f v args))))

(defn assoc!
  ([tcoll key val]
   (cljs.core/assoc! tcoll key val))
  ([tcoll key val & kvs]
   (let [ret (cljs.core/assoc! tcoll key val)]
     (if kvs
       (recur ret (first kvs) (second kvs) (nnext kvs))
       ret))))

(defn dissoc!
  ([tcoll key]
   (cljs.core/dissoc! tcoll key))
  ([tcoll key & keys]
   (let [ret (cljs.core/dissoc! tcoll key)]
     (if keys
       (recur ret (first keys) (next keys))
       ret))))
