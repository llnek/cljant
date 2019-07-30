;; Copyright (c) 2013-2019, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.antclj.test

  (:require [czlab.antclj.antlib :as a :refer [uid]]
            [clojure.string :as cs]
            [clojure.java.io :as io])

  (:use [clojure.test])

  (:import [java.io File]
           [java.nio.file Files]
           [java.rmi.server UID]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
(def ^:private tmpdir (io/file (System/getProperty "java.io.tmpdir")))
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
(defn- ctf<>
  {:tag File}
  ([] (ctf<> nil))
  ([d] (io/file (or d tmpdir) (uid))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ctd<>
  {:tag File}
  ([] (ctd<> nil))
  ([d] (doto (ctf<> d) (.mkdirs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest czlabtestantclj-test
  (is (let [f (ctf<>)
            _ (spit f "yada yada some-token yoda yoda")
            _ (a/run*
                (a/replace
                  {:file (.getCanonicalPath f)}
                  [[:replacetoken "some-token"]
                   [:replacevalue "some-value"]]))
            s (slurp f)]
        (cs/includes? s "some-value")))

  (is (let [_ (a/run* (a/hostinfo
                        {:host "www.google.com"
                         :prefix "goog"}))
            m (a/read-properties*)
            ps (filter #(cs/starts-with? % "goog.") (keys m))]
        (boolean (not-empty ps))))

  (is (let [f (.getCanonicalPath (ctf<>))
            _ (a/run* (a/echoproperties {:destfile f}))]
        (.exists (io/file f))))

  (is (let [f (.getCanonicalPath (ctf<>))
            _ (a/run* (a/get {:src "http://google.com"
                              :dest f
                              :ignoreerrors true
                              :quiet true}))]
        (.exists (io/file f))))

  (is (let [f (.getCanonicalPath (ctf<>))
            _ (a/run* (a/genkey
                        {:storepass "password"
                         :keystore f
                         :alias "mykey"}
                        [[:dname
                          [[:param {:name "CN" :value "Joe Blogg"}]
                           [:param {:name "OU" :value "Acme Lab"}]
                           [:param {:name "O" :value "Acme Inc."}]
                           [:param {:name "C" :value "US"}]]]]))]
        (.exists (io/file f))))

  (is (let [d (ctd<>)
            f (ctf<>)
            n (.getName f)
            z (io/file d n)
            _ (spit f "hello")
            _ (a/copy-file* f d)
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
            _ (a/move-file* f d)
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
            _ (a/move-file* f d)
            ok (= "hello" (slurp z))
            _ (a/clean-dir* d)]
        (and ok
             (not (.exists z)))))

  (is (let [d (ctd<>)
            f (ctf<>)
            n (.getName f)
            z (io/file d n)
            _ (spit f "hello")
            _ (a/move-file* f d)
            ok (= "hello" (slurp z))
            _ (a/delete-dir* d)]
        (and ok
             (not (.exists d)))))

  #_
  (is (let [f (ctf<>)
            g (ctf<>)
            _ (spit f "hello")
            _ (a/create-link* (.getCanonicalPath g)
                              (.getCanonicalPath f))
            ok (and (= "hello" (slurp g))
                    (Files/isSymbolicLink (.toPath g)))
            _ (a/delete-link* (.getCanonicalPath g))
            ok2 (not (.exists g))]
        (and ok ok2)))

  (is (let [root (ctd<>)
            src (ctd<> root)
            out (ctd<>)
            _ (.mkdirs src)
            tn (uid)
            f (io/file src "Test.java")
            _ (spit f javacode)
            _
            (a/run* (a/javac {:srcdir (.getCanonicalPath root)
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
                                 {:location (.getCanonicalPath ^File tmpdir)}]
                                [:fileset {:dir tmpdir
                                           :includes {:name "**/*.jar"}}]]]])
                    (a/sleep {:seconds "2"})
                    (a/java {:classname "x.Test"
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

  (is (string? "end test")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

