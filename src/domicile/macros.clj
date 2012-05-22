(ns domicile.macros)

(defmacro ?call
  [jobj prop & args]
  `(when-let [method# (aget ~jobj ~(name prop))]
     (. method# ~'call ~jobj ~@args)))

(defmacro ?apply
  [jobj prop & args]
  `(when-let [method# (aget ~jobj ~(name prop))]
     (. method# ~'apply ~jobj
        (apply cljs.core/array
               (apply cljs.core/list* '~args)))))
