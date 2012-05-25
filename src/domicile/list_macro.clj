(ns domicile.list-macro)

(defmacro extend-dom-list
  [list-type]
  `(extend-type ~list-type
     cljs.core/ISeqable
     (~'-seq
       [list#]
       (for [i# (range 0 (. list# -length))]
         (. list# ~'item i#)))

     cljs.core/ICounted
     (~'-count
       [list#]
       (. list# -length))

     cljs.core/IReduce
     (~'-reduce
       ([list# f#]
        (~'ci-reduce list# f#))
       ([list# f# start#]
        (~'ci-reduce list# f# start#)))

     cljs.core/IIndexed
     (~'-nth
       ([list# n#]
        (when (< n# (. list# -length))
          (. list# ~'item n#)))
       ([list# n# not-found#]
        (if (< n# (. list# -length))
          (. list# ~'item n#)
          not-found#)))))
