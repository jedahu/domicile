(ns domicile.macros-test
  (:require
    [menodora.core :as mc])
  (:use
    [domicile.util :only [update!]]
    [menodora.predicates :only [eq]])
  (:use-macros
    [menodora :only [defsuite describe should expect]]
    [domicile.macros :only [?call ?apply japply set-change!]]))

(defsuite macros-tests
  (describe "japply"
    (should "apply method to args"
      (expect eq "ASDF" (japply "asdf" toUpperCase))
      (expect eq "asdf12" (japply "asdf" concat [1 2]))
      (expect eq "asdf123" (japply "asdf" concat 1 [2 3]))
      (let [rest [2 3]]
        (expect eq "asdf23" (japply "asdf" concat rest))
        (expect eq "asdf123" (japply "asdf" concat 1 rest))))
    (should "throw error when last arg is not a sequence"
      (expect eq :error (try (japply "asdf" concat 2 1)
                          (catch js/Object e :error))))
    (should "evaluate args only once"
      (let [obj (atom 0)
            args (atom 0)]
        (expect eq "abc123"
          (japply (do (swap! obj inc) "abc")
                  concat
                  (do (swap! args inc) [1 2 3])))
        (expect eq 1 @obj)
        (expect eq 1 @args))))

  (describe "?call"
    (should "do nothing when method not present"
      (expect eq nil (?call "asdf" missingMethod))
      (expect eq nil (?call "asdf" missingMethod 1 2 3)))
    (should "call method when present"
      (expect eq "ASDF" (?call "asdf" toUpperCase))
      (expect eq \a (?call "asdf" charAt 0))))

  (describe "?apply"
    (should "do nothing when method not present"
      (expect eq nil (?apply "asdf" missingMethod))
      (expect eq nil (?apply "asdf" missingMethod 1 [2 3])))
    (should "apply method when present"
      (expect eq "ASDF" (?apply "asdf" toUpperCase))
      (expect eq "asdf12" (?apply "asdf" concat [1 2]))
      (expect eq "asdf123" (?apply "asdf" concat 1 [2 3]))
      (let [rest [2 3]]
        (expect eq "asdf23" (?apply "asdf" concat rest))
        (expect eq "asdf123" (?apply "asdf" concat 1 rest))))
    (should "throw error when last arg is not a sequence"
      (expect eq :error (try (?apply "asdf" concat 2 1)
                          (catch js/Object e :error)))))

  (describe "set-change!"
    (should "operate like swap!"
      (expect eq 3 (set-change! (js-obj "number" 2) number inc))
      (let [jobj (js-obj "number" 2)]
        (expect eq 9 (set-change! jobj number + 3 4))
        (expect eq 9 (. jobj -number))))
    (should "evaluate args only once"
      (let [obj (atom 0)]
        (expect eq 3
          (set-change! (do (swap! obj inc) (js-obj "number" 2))
                       number
                       inc))
        (expect eq 1 @obj))))

  (describe "update!"
    (should "operate like update-in"
      (let [map (transient {:number 1})]
        (expect eq map (update! map :number inc))
        (expect eq 2 (:number map))
        (expect eq map (update! map :number + 3 4))
        (expect eq 9 (:number map))))))

;;. vim: set lispwords+=defsuite,describe,should,expect:
