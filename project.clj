(defproject advent-of-code-2021 "0.1.0-SNAPSHOT"
  :description "Advent of code 2021"
  :dependencies [[org.clojure/clojure "1.10.3"] [org.clojure/data.priority-map "1.1.0"]]
  :main ^:skip-aot se.lixi.adventofcode2021.core
  :target-path "target/%s"
  :profiles {:uberjar {:aot      :all
                       :jvm-opts ["-Dclojure.compiler.direct-linking=true"]}})
