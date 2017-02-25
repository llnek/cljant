;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.antclj.test

  (:require [czlab.antclj.antlib :as a]
            [clojure.string :as cs]
            [clojure.java.io :as io])

  (:use [clojure.test])

  (:import [java.io File]
           [java.nio.file Files]
           [java.rmi.server UID]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;(set! *warn-on-reflection* true)
(def ^:private tmpdir (io/file (System/getProperty "java.io.tmpdir")))
(defn- uid "" ^String [] (.replaceAll (str (UID.)) "[:\\-]+" ""))
(defn- ctf<> "" ^File [& [d]] (io/file (or d tmpdir) (uid)))
(defn- ctd<> "" ^File [& [d]] (doto (ctf<> d) (.mkdirs)))
(def ^:private javac (str
                       (System/getenv "JAVA_HOME") "/bin/javac"))
(def
  ^:private
  javacode
  "package x;
  public class Test  {
  public static void main(String[] args) {
  try {
  java.io.File f = new java.io.File(System.getProperty(\"java.io.tmpdir\") + \"/\" + args[0]);
  java.nio.file.Files.write(f.toPath(), \"hello\".getBytes(\"utf-8\"),
        java.nio.file.StandardOpenOption.CREATE);
    } catch (Throwable t) { t.printStackTrace(); }
  }
  }")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(deftest czlabtestantclj-test

  #_
  (is (let [_
            (runTasks*
              (antGenkey {} []))]
        true))

  (is (let [d (ctd<>)
            f (ctf<>)
            n (.getName f)
            z (io/file d n)
            _ (spit f "hello")
            _ (a/copyFile f d)
            ok (= "hello" (slurp z))]
        (io/delete-file z true)
        (io/delete-file f true)
        (.delete d)
        ok))

  (is (let [d (ctd<>)
            f (ctf<>)
            n (.getName f)
            z (io/file d n)
            _ (spit f "hello")
            _ (a/moveFile f d)
            ok (and (= "hello" (slurp z))
                    (not (.exists f)))]
        (io/delete-file z true)
        (.delete d)
        ok))

  (is (let [d (ctd<>)
            f (ctf<>)
            n (.getName f)
            z (io/file d n)
            _ (spit f "hello")
            _ (a/moveFile f d)
            ok (= "hello" (slurp z))
            _ (a/cleanDir d)]
        (and ok
             (not (.exists z)))))

  (is (let [d (ctd<>)
            f (ctf<>)
            n (.getName f)
            z (io/file d n)
            _ (spit f "hello")
            _ (a/moveFile f d)
            ok (= "hello" (slurp z))
            _ (a/deleteDir d)]
        (and ok
             (not (.exists d)))))

  (is (let [f (ctf<>)
            g (ctf<>)
            _ (spit f "hello")
            _ (a/createLink (.getCanonicalPath g)
                            (.getCanonicalPath f))
            ok (and (= "hello" (slurp g))
                    (Files/isSymbolicLink (.toPath g)))
            _ (a/deleteLink (.getCanonicalPath g))
            ok2 (not (.exists g))]
        (and ok ok2)))

  (is (let [root (ctd<>)
            src (ctd<> root "x")
            out (ctd<>)
            _ (.mkdirs src)
            tn (uid)
            f (io/file src "Test.java")
            _ (spit f javacode)
            _
            (a/runTasks*
              (a/javac
                {:srcdir (.getCanonicalPath root)
                 :destdir (.getCanonicalPath out)
                 :target "8"
                 :executable javac
                 :debugLevel "lines,vars,source"
                 :includeantruntime false
                 :debug true
                 :fork true}
                [[:compilerarg {:line "-Xlint:deprecation"}]
                 [:include {:name "**/*.java"}]
                 [:classpath
                  {}
                  [[:path
                    {:location (.getCanonicalPath tmpdir)}]
                   [:fileset {:dir tmpdir
                              :includes {:name "**/*.jar"}}]]]])
              (a/sleep {:seconds "2"})
              (a/java
                {:classname "x.Test"
                 :fork true
                 :failonerror true}
                [[:arg {:value tn}]
                 [:classpath
                  {}
                  [[:path
                    {:location (.getCanonicalPath out)}]]]])
              (a/sleep {:seconds "2"}))
            z (io/file tmpdir tn)]
        (= "hello" (slurp z))))

  (is (string? "that's all folks!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

