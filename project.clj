(defproject org.dave/cfg "1.0.0"
  :description "Manage your options, holmes."
  :url "http://github.com/ds300/cfg"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}
  :dependencies [[org.clojure/clojure "1.4.0"]]
  :profiles {
    :user {:plugins [[lein-midje "3.0.0"]]}
    :dev {
      :dependencies [[midje "1.5.0"]]
    }
  })
