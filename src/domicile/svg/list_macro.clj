(ns domicile.svg.list-macro)

(defmacro extend-svg-list
  [list-type make-val get-val]
  `(extend-type ~list-type
     cljs.core/ISeqable
     (~'-seq
       [list#]
       (for [i# (range 0 (. list# -numberOfItems))]
         (~get-val (. list# ~'getItem i#))))

     cljs.core/ICounted
     (~'-count
       [list#]
       (. list# -numberOfItems))

     cljs.core/ITransientAssociative
     (~'-assoc!
       [list# key# val#]
       (~'-assoc-n! list# key# val#))

     cljs.core/ITransientCollection
     (~'-conj!
       [list# val#]
       (. list# ~'appendItem (~make-val val#))
       list#)
     (~'-persistent!
       [list#]
       (vec (~'-seq list#)))

     cljs.core/ITransientVector
     (~'-assoc-n!
       [list# n# val#]
       (. list# ~'replaceItem (~make-val val#) n#)
       list#)
     (~'-pop!
       [list#]
       (. list# ~'removeItem (dec (. list# -numberOfItems)))
       list#)

     cljs.core/IIndexed
     (~'-nth
       ([list# n#]
        (when (< n# (. list# -numberOfItems))
          (~get-val (. list# ~'getItem n#))))
       ([list# n# not-found#]
        (if (< n# (. list# -numberOfItems))
          (~get-val (. list# ~'getItem n#))
          not-found#)))

     cljs.core/IReduce
     (~'-reduce
       ([list# f#]
        (~'ci-reduce list# f#))
       ([list# f# start#]
        (~'ci-reduce list# f# start#)))))
