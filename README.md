# antclj

[![Build Status](https://travis-ci.org/llnek/antclj.svg?branch=master)](https://travis-ci.org/llnek/antclj)

[Apache ant][1] is a venerable java build tool providing numerous useful and 
robust tasks, ranging from file system operations to os operations.  **antclj** 
provides a simple way to access those tasks in clojure by treating each as a function.

## Installation

Add the following dependency to your `project.clj` file:

    [io.czlab/antclj "1.0.0"]

## Documentation

* [Task Docs](http://ant.apache.org/manual/index.html)

## Usage

```clojure
(ns demo.core
  (:require [czlab.antclj.antlib :as a]))

(defn compileAndRun [srcDir destDir]
  (a/run*
    (a/javac
      {:srcdir srcDir
       :destdir destDir
       :target "8"
       :executable "/bin/javac"
       :debugLevel "lines,vars,source"
       :includeantruntime false
       :debug true
       :fork true}
      [[:compilerarg {:line "-Xlint:deprecation"}]
       [:include {:name "**/*.java"}]
       [:classpath
        [[:path {:location "/dev/classes"}]
         [:fileset {:dir "/home/joe/maven"
                    :includes {:name "**/*.jar"}}]]]])
    (a/sleep {:seconds "2"})
    (a/java
      {:classname "demo.App"
       :fork true
       :failonerror true}
      [[:arg {:value "argvalue1"}]
       [:classpath
        [[:path {:location destDir}]]]])
    (a/sleep {:seconds "2"})))

```

## Contacting me / contributions

Please use the project's [GitHub issues page] for all questions, ideas, etc. **Pull requests welcome**. See the project's [GitHub contributors page] for a list of contributors.

## License

Copyright © 2013-2017 Kenneth Leung

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.

<!--- links -->
[1]: http://ant.apache.org/
<!--- links (repos) -->
[CHANGELOG]: https://github.com/llnek/antclj/releases
[GitHub issues page]: https://github.com/llnek/antclj/issues
[GitHub contributors page]: https://github.com/llnek/antclj/graphs/contributors

