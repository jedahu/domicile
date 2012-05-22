(defproject domicile "0.1.1-SNAPSHOT"
  :description "A clojurescript DOM library"
  :url "https://github.com/jedahu/domicile"
  :license {:name "ISC"
            :url "http://opensource.org/licenses/isc-license.txt"}

  :profiles
  {:dev
   {:dependencies [[cst "0.3.1"]
                   [menodora "0.1.4"]]}}

  :plugins [[lein-cst "0.3.1"]]

  :cst
  {:suites [domicile.core-test/core-tests
            domicile.create-test/create-tests
            domicile.macros-test/macros-tests]})
