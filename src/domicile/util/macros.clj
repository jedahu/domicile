(ns domicile.util.macros)

(defmacro japply
  [jobj prop & args]
  (if (seq args)
    `(let [jobj# ~jobj
           method# (aget jobj# ~(name prop))]
       (. method# ~'apply jobj#
          (apply cljs.core/array
                 (cljs.core/list* ~@args))))
    `(. ~jobj ~prop)))

(defmacro ?call
  [jobj prop & args]
  `(when-let [method# (aget ~jobj ~(name prop))]
     (. method# ~'call ~jobj ~@args)))

(defmacro ?apply
  [jobj prop & args]
  `(let [jobj# ~jobj]
     (when (aget jobj# ~(name prop))
       (japply jobj# ~prop ~@args))))

(defmacro set-change!
  [obj prop f & args]
  (let [prop (name prop)]
    `(let [obj# ~obj
           val# (aget obj# ~prop)]
       (aset obj# ~prop (~f val# ~@args)))))
