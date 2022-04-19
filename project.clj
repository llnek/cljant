;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defproject io.czlab/cljant "2.1.0"

  ;;add these for maven deployment
  :Xpom-addition
  [:developers [:developer
                [:name "Kenneth Leung"]
                [:url "https://github.com/llnek"]]]
  ;;trick lein to generate javadoc and sources artifacts
  :Xclassifiers {:javadoc {} :sources {}}
  :Xdeploy-repositories
  [["releases" {:url "https://oss.sonatype.org/service/local/staging/deploy/maven2/"}
    "snapshots" {:url "https://oss.sonatype.org/content/repositories/snapshots/"}]]
  ;;for maven deployment

  :license {:url "https://www.apache.org/licenses/LICENSE-2.0.txt"
            :name "Apache License"}

  :description "Apache ant wrapped in clojure."
  :url "https://github.com/llnek/cljant"

  :dependencies [;;;;[ant-contrib/ant-contrib "1.0b3" :exclusions [ant]]
                 ;;;;[org.apache.ant/ant-launcher "1.10.1"]
                 [org.apache.ant/ant-junit4 "1.10.12" :scope "test"]
                 [org.apache.ant/ant-junit "1.10.12" :scope "test"]
                 [junit/junit "4.13.2" :scope "test"]
                 [org.apache.ant/ant "1.10.12"]]

  :plugins [[cider/cider-nrepl "0.28.3"]
            [lein-codox "0.10.8"]
            [lein-cljsbuild "1.1.8"]]

  :profiles {:provided {:dependencies
                        [[org.clojure/clojure "1.11.1" :scope "provided"]
                         [net.mikera/cljunit "0.7.0" :scope "test"]]}
             :uberjar {:aot :all}}

  :global-vars {*warn-on-reflection* true}
  :target-path "out/%s"
  :omit-source true
  :aot :all
  :coordinate! "czlab"

  :java-source-paths ["src/main/java" "src/test/java"]
  :source-paths ["src/main/clojure"]
  :test-paths ["src/test/clojure"]

  :jvm-opts ["-Dlog4j.configurationFile=file:attic/log4j2.xml"]
  :javac-options ["-source" "16"
                  "-target" "16"
                  "-Xlint:unchecked" "-Xlint:-options" "-Xlint:deprecation"])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


