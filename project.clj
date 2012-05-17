(defproject domicile "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :profiles
  {:dev
   {:dependencies [[cst "0.3.1"]
                   [menodora "0.1.4"]]}}

  :plugins [[lein-cst "0.3.1"]]

  :cst
  {:suites [domicile.core-test/core-tests]})
