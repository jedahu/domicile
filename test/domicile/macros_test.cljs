(ns domicile.macros-test
  (:require
    [menodora.core :as mc])
  (:use
    [menodora.predicates :only [eq]])
  (:use-macros
    [menodora :only [defsuite describe should expect]]
    [domicile.macros :only [?call ?apply]]))

(defsuite macros-tests
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
                          (catch js/Object e :error))))))

;;. vim: set lispwords+=defsuite,describe,should,expect:
