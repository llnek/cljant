;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defproject io.czlab/antclj "1.0.3"

  ;;add these for maven deployment
  :pom-addition
  [:developers [:developer
                [:name "Kenneth Leung"]
                [:url "https://github.com/llnek"]]]
  ;;trick lein to generate javadoc and sources artifacts
  :classifiers {:javadoc {} :sources {}}
  :deploy-repositories
  [["releases" {:url "https://oss.sonatype.org/service/local/staging/deploy/maven2/"}
    "snapshots" {:url "https://oss.sonatype.org/content/repositories/snapshots/"}]]
  ;;for maven deployment

  :license {:url "http://www.eclipse.org/legal/epl-v10.html"
            :name "Eclipse Public License"}
  :url "https://github.com/llnek/antclj"

  :description "Apache ant wrapped in clojure."

  :dependencies [;;;;[ant-contrib/ant-contrib "1.0b3" :exclusions [ant]]
                 ;;;;[org.apache.ant/ant-launcher "1.10.1"]
                 [org.apache.ant/ant-junit4 "1.10.1" :scope "test"]
                 [org.apache.ant/ant-junit "1.10.1" :scope "test"]
                 [junit/junit "4.12" :scope "test"]
                 [org.apache.ant/ant "1.10.1"]]

  :plugins [[cider/cider-nrepl "0.14.0"]
            [lein-cprint "1.2.0"]
            [lein-codox "0.10.3"]]

  :profiles {:provided {:dependencies
                        [[org.clojure/clojure "1.8.0" :scope "provided"]
                         [net.mikera/cljunit "0.6.0" :scope "test"]]}
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
  :javac-options ["-source" "7"
                  "-Xlint:unchecked" "-Xlint:-options" "-Xlint:deprecation"])


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


