(defproject io.czlab/antclj "0.1.0"

  :description "Apache ant wrapped in clojure"
  :url "https://github.com/llnek/antclj"

  :license {:name "Apache License 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}

  :dependencies [;;[ant-contrib/ant-contrib "1.0b3" :exclusions [ant]]
                 ;;[org.apache.ant/ant-launcher "1.9.7"]
                 [org.apache.ant/ant-junit4 "1.9.7"]
                 [org.apache.ant/ant-junit "1.9.7"]
                 [org.apache.ant/ant "1.9.7"]]

  :plugins [[lein-pprint "1.1.2"]
            [lein-czlab "0.1.0"]
            [lein-codox "0.10.2"]]

  :profiles {:provided {:dependencies
                        [[net.mikera/cljunit "0.6.0" :scope "test"]
                         [junit/junit "4.12" :scope "test"]
                         [org.clojure/clojure "1.8.0" :scope "provided"]
                         [codox/codox "0.10.2" :scope "provided"]]}
             :uberjar {:aot :all}}

  :global-vars {*warn-on-reflection* false}
  :target-path "out/%s"
  :aot :all

  :hooks [leiningen.lein-czlab]
  :root-package "czlab"

  :java-source-paths ["src/main/java" "src/test/java"]
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]
  :resource-paths ["src/main/resources"]

  :jvm-opts ["-Dlog4j.configurationFile=file:attic/log4j2.xml"]
  :javac-options ["-source" "8"
                  "-Xlint:unchecked" "-Xlint:-options" "-Xlint:deprecation"])


