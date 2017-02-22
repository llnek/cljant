;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns czlab.test.antclj.test

  (:require [clojure.string :as cs]
            [clojure.java.io :as io])

  (:use [czlab.antclj.antlib]
        [clojure.test])

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

  (is (let [d (ctd<>)
            f (ctf<>)
            n (.getName f)
            z (io/file d n)
            _ (spit f "hello")
            _ (copyFile f d)
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
            _ (moveFile f d)
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
            _ (moveFile f d)
            ok (= "hello" (slurp z))
            _ (cleanDir d)]
        (and ok
             (not (.exists z)))))

  (is (let [d (ctd<>)
            f (ctf<>)
            n (.getName f)
            z (io/file d n)
            _ (spit f "hello")
            _ (moveFile f d)
            ok (= "hello" (slurp z))
            _ (deleteDir d)]
        (and ok
             (not (.exists d)))))

  (is (let [f (ctf<>)
            g (ctf<>)
            _ (spit f "hello")
            _ (symLink (.getCanonicalPath g)
                       (.getCanonicalPath f))
            ok (and (= "hello" (slurp g))
                    (Files/isSymbolicLink (.toPath g)))
            _ (symUnlink (.getCanonicalPath g))
            ok2 (not (.exists g))]
        (and ok ok2)))

  (is (string? "that's all folks!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

