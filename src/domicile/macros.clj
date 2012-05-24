(ns domicile.macros)

(defmacro japply
  [jobj prop & args]
  `(let [method# (aget ~jobj ~(name prop))]
     (. method# ~'apply ~jobj
        (apply cljs.core/array
               (cljs.core/list* ~@args)))))

(defmacro ?call
  [jobj prop & args]
  `(when-let [method# (aget ~jobj ~(name prop))]
     (. method# ~'call ~jobj ~@args)))

(defmacro ?apply
  [jobj prop & args]
  `(when-let [method# (aget ~jobj ~(name prop))]
     (. method# ~'apply ~jobj
        (apply cljs.core/array
               (cljs.core/list* ~@args)))))

(defmacro set-change!
  [obj prop f & args]
  (let [prop (name prop)]
    `(let [obj# ~obj
           val# (aget obj# ~prop)]
       (aset obj# ~prop (apply ~f val# ~@args)))))
