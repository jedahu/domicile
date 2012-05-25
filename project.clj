(defproject domicile "0.4.2-SNAPSHOT"
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
  {:suites [domicile.dom-test/dom-tests
            domicile.extended-types-test/extended-types-tests
            domicile.create-test/create-tests
            domicile.util-test/util-tests
            domicile.svg-test/svg-tests
            domicile.svg.extended-types-test/svg-extended-types-tests
            domicile.css-test/css-tests]})
