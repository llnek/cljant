(defproject czlab/czlab-antclj "0.1.0"

  :description "Apache ant wrapped in clojure"
  :url "https://github.com/llnek/antclj"

  :license {:name "Apache License 2.0"
            :url "http://www.apache.org/licenses/LICENSE-2.0"}

  :dependencies [;;[ant-contrib/ant-contrib "1.0b3" :exclusions [ant]]
                 ;;[org.apache.ant/ant-launcher "1.9.7"]
                 [org.apache.ant/ant-junit4 "1.9.7"]
                 [org.apache.ant/ant-junit "1.9.7"]
                 [org.apache.ant/ant "1.9.7"]]

  :profiles {:provided {:dependencies
                        [[net.mikera/cljunit "0.6.0" :scope "test"]
                         [junit/junit "4.12" :scope "test"]
                         [org.clojure/clojure "1.8.0" :scope "provided"]
                         [codox/codox "0.10.2" :scope "provided"]]}
             :uberjar {:aot :all}}

  :global-vars {*warn-on-reflection* true}

  :java-source-paths ["src/main/java"]
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]

  :javac-options ["-target" "1.8" "-source" "1.8" "-Xlint:-options"])

