;; Licensed under the Apache License, Version 2.0 (the "License");
;; you may not use this file except in compliance with the License.
;; You may obtain a copy of the License at
;;
;;     http://www.apache.org/licenses/LICENSE-2.0
;;
;; Unless required by applicable law or agreed to in writing, software
;; distributed under the License is distributed on an "AS IS" BASIS,
;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;; See the License for the specific language governing permissions and
;; limitations under the License.
;;
;; Copyright © 2013-2022, Kenneth Leung. All rights reserved.

(ns
  czlab.test.cljant.test

  (:require [czlab.cljant.antlib :as a :refer [uid]]
            [clojure.string :as cs]
            [clojure.java.io :as io])

  (:use [clojure.test])

  (:import [java.io File]
           [java.nio.file Files]
           [java.rmi.server UID]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
(def ^:private
  tmpdir (io/file (System/getProperty "java.io.tmpdir")))
(def ^:private
  javac (str (System/getenv "JAVA_HOME") "/bin/javac"))
(def ^:private
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

;(println (str "------- " tmpdir))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ctf<>
  "Create temp file."
  {:tag File}
  ([] (ctf<> nil))
  ([d] (io/file (or d tmpdir) (uid))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ctd<>
  "Create temp dir."
  {:tag File}
  ([] (ctd<> nil))
  ([d] (doto (ctf<> d) (.mkdirs))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(deftest czlabtestcljant-test

  (is (let [f (ctf<>)
            y "yada yada some-token yoda yoda"]
        (spit f y)
        (a/run*
          (a/replace
            {:file (.getCanonicalPath f)}
            [[:replacetoken "some-token"]
             [:replacevalue "some-value"]]))
        (= (slurp f) "yada yada some-value yoda yoda")))

  (is (let []
        (a/run* (a/hostinfo
                  {:host "www.google.com"
                   :prefix "goog"}))
        (->> (a/read-properties*)
             keys
             (filter #(cs/starts-with? % "goog."))
             not-empty)))

  (is (let [f (.getCanonicalPath (ctf<>))]
        (a/run* (a/echoproperties {:destfile f}))
        (.exists (io/file f))))

  (is (let [f (.getCanonicalPath (ctf<>))]
        (a/run* (a/get {:src "https://google.com"
                        :dest f
                        :quiet true
                        :ignoreerrors true}))
        (> (count (slurp f)) 100)))

  (comment "ant is broken, genkey is not a valid arg for underlying keytool"
  (is (let [f (.getCanonicalPath (ctf<>))]
        (a/run* (a/genkey
                  {:storepass "password" :keystore f :alias "mykey"}
                  [[:dname
                    [[:param {:name "CN" :value "Joe Blogg"}]
                     [:param {:name "OU" :value "Acme Lab"}]
                     [:param {:name "O" :value "Acme Inc."}]
                     [:param {:name "C" :value "US"}]]]]))
        (> (.length (io/file f)) 100)))
)

  (is (let [d (ctd<>)
            f (ctf<>)
            n (.getName f)
            z (io/file d n)]
        (spit f "hello")
        (a/copy-file* f d)
        (try (= "hello" (slurp z))
             (finally (io/delete-file z true)
                      (io/delete-file f true)
                      (.delete d)))))

  (is (let [d (ctd<>)
            f (ctf<>)
            n (.getName f)
            z (io/file d n)]
        (spit f "hello")
        (a/move-file* f d)
        (try (and (= "hello" (slurp z))
                  (not (.exists f)))
             (finally (io/delete-file z true)
                      (.delete d)))))

  (is (let [d (ctd<>)
            f (ctf<>)
            n (.getName f)
            z (io/file d n)
            _ (spit f "hello")
            _ (a/move-file* f d)
            ok (= "hello" (slurp z))]
        (a/clean-dir* d)
        (and ok (not (.exists z)))))

  (is (let [d (ctd<>)
            f (ctf<>)
            n (.getName f)
            z (io/file d n)
            _ (spit f "hello")
            _ (a/move-file* f d)
            ok (= "hello" (slurp z))]
        (a/delete-dir* d)
        (and ok (not (.exists d)))))

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
            src (io/file root "x")
            out (ctd<> root)
            _ (.mkdirs out)
            _ (.mkdirs src)
            tn (uid)
            f (io/file src "Test.java")
            _ (spit f javacode)
            _
            (a/run* (a/javac {:srcdir (.getCanonicalPath root)
                              :destdir (.getCanonicalPath out)
                              ;:target "11"
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
                    (a/sleep {:seconds "2"}))]
        (= "hello" (slurp (io/file tmpdir tn)))))

  (is (string? "end test")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

