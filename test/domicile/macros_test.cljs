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
      (expect eq "asdfxy" (?apply "asdf" concat ["x" "y"]))
      (expect eq "asdfxy1" (?apply "asdf" concat "x" ["y" 1])))
    (should "throw error when last arg is not a sequence"
      (expect eq :error (try (?apply "asdf" concat "x" 1)
                          (catch js/Object e :error))))))

;;. vim: set lispwords+=defsuite,describe,should,expect:
