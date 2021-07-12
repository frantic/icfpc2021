(defproject icfpc2021 "0.1.0-SNAPSHOT"
  :description "ICFPC2021 Editor"
  :url "https://frantic.im"
  :dependencies [[org.clojure/clojure "1.10.1"]
                 [org.clojure/data.json "2.3.1"]
                 [clj-http "3.12.3"]]
  :repl-options {:init-ns icfpc2021.core}
  :main icfpc2021)
