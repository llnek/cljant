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
;; Copyright © 2013-2024, Kenneth Leung. All rights reserved.

(ns

  czlab.cljant.antlib

  "Apache Ant project & task wrappers.
  The anatomy of an ant task is a xml construct,
  where the attributes are termed as options and
  nested elements are treated as vectors inside of a vector."

  (:import [org.apache.tools.ant.taskdefs.optional.unix Symlink]
           [org.apache.tools.ant.types AbstractFileSet]
           [org.apache.tools.ant.taskdefs Delete]
           [java.beans
            FeatureDescriptor
            MethodDescriptor
            Introspector
            PropertyDescriptor]
           [java.lang.reflect Constructor Method]
           [java.util Map]
           [java.io File]
           [org.apache.tools.ant.listener
            AnsiColorLogger
            TimestampedLogger]
           [org.apache.tools.ant
            IntrospectionHelper
            ProjectComponent
            NoBannerLogger
            Project
            Target
            Task]
           [java.rmi.server UID]
           [clojure.lang APersistentMap])

  (:require [clojure.java.io :as io]
            [clojure.core :as cc]
            [clojure.string :as cs])

  (:refer-clojure :exclude [apply get sync concat replace]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
(def ^:private tmpdir (io/file (System/getProperty "java.io.tmpdir")))
(defn uid

  "Generate an unique identifier."
  {:tag String
   :arglists '([])}
  []

  (.replaceAll (str (UID.)) "[:\\-]+" ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ^:private do-with
  [bindings & more]
  (assert (== 2 (count bindings)))
  (let [a (first bindings)
        b (last bindings)] `(let [~a ~b] ~@more ~a)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(declare cfg-nested)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private skipped-tasks #{"ant" "antcall" "import" "include"
                               "copydir" "copyfile" "copypath"
                               "deltree" "execon" "javadoc2"
                               "jlink" "jspc" "mimemail"
                               "rename" "renameext" "filter"
                               "antstructure" "antversion"})

(def ^:private pred-t (constantly true))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ^:private nth??
  [c p] `(first (drop (dec ~p) ~c)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ^:private trap!
  [& xs] `(throw (Exception. ~@xs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ctor!

  "Create an object from class, and set it to point to the project."
  [^Class cz ^Project pj]

  (let
    [^Constructor
     c0 (try (->> (make-array Class 0)
                  (.getConstructor cz))
             (catch Throwable _))
     ^Constructor
     c1 (if (nil? c0)
          (try (->> [Project]
                    (into-array Class)
                    (.getConstructor cz))
               (catch Throwable _)))]
    (doto
      (or (some-> c0 (.newInstance (make-array Object 0)))
          (some-> c1 (.newInstance (into-array Object [pj]))))
      (some->> (.setProjectReference pj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;better colors that are not *dimmed*
(def ^:private _ansi-logger_
  (let [f (io/file tmpdir "czlab-antlogansi.colors")
        s (cs/join "\n"
                   ["AnsiColorLogger.ERROR_COLOR=0;31"
                    "AnsiColorLogger.WARNING_COLOR=0;35"
                    "AnsiColorLogger.INFO_COLOR=0;36"
                    "AnsiColorLogger.VERBOSE_COLOR=0;32"
                    "AnsiColorLogger.DEBUG_COLOR=0;34"])]
    (if-not (.exists f) (spit f s))
    (System/setProperty "ant.logger.defaults"
                        (.getCanonicalPath f))
    (doto (AnsiColorLogger.)
      (.setOutputPrintStream System/out)
      (.setErrorPrintStream System/err)
      (.setMessageOutputLevel Project/MSG_INFO))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- project<>
  ^Project [] (doto (Project.) .init (.setName "projx")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- exec-target
  [^Target t] (-> (.getProject t)
                  (.executeTarget (.getName t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;trick to type hint to avoid reflection warning
(defmacro ^:private gfdn
  [d]
  `(.getName ~(with-meta d {:tag 'FeatureDescriptor})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private create-opstrs ["addConfigured" "add" "create"])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def ^:private create-ops (zipmap create-opstrs
                                  (mapv #(count %) create-opstrs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- clj-map

  ([m] (clj-map m pred-t))

  ([m pred]
   (persistent!
     (reduce
       #(let [[k v] %2]
          (if (pred k v) (assoc! %1 k v) %1)) (transient {}) m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defonce ^:private beans-cooked? (atom false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;create a default project.
(defonce ^:private dftprj (atom (project<>)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Get a list of task definitions.
(def ^:private _tasks
  (clj-map
    (.getTaskDefinitions ^Project @dftprj)
    (fn [k v] (not (contains? skipped-tasks k)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Get all the type definitions.
(def ^:private _types
  (clj-map (.getDataTypeDefinitions ^Project @dftprj )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- tst-spec-ops?

  "Test for special methods which aggregates nested elements."
  [^MethodDescriptor d]

  (let
    [pms (.. d getMethod getParameterTypes)
     mn (gfdn d)
     pc (count pms)]
    (or (and (cs/starts-with? mn "create") (== 0 pc))
        (and (cs/starts-with? mn "add") (== 1 pc))
        (and (cs/starts-with? mn "addConfigured") (== 1 pc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- get-creator-info
  [descs]
  (persistent!
    (reduce
      #(if (tst-spec-ops? %2)
         (assoc! %1
                 (cs/lower-case (gfdn %2)) %2) %1) (transient {}) descs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- get-binfo
  [descs]
  (persistent!
    (reduce
      #(assoc! %1 (keyword (gfdn %2)) %2) (transient {}) descs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- get-bean-info
  [^Class cz]
  (let [b (Introspector/getBeanInfo cz)]
    {:props (get-binfo (.getPropertyDescriptors b))
     ;;:ops (get-binfo (.getMethodDescriptors b))
     :aggrs (get-creator-info (.getMethodDescriptors b))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- beanie
  [m]
  (persistent!
    (reduce
      #(let [[_ v] %2]
         (assoc! %1 v (get-bean-info v))) (transient {}) m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;cache bean-info of class
(if-not @beans-cooked?
  (do
    (def
      ^:private _beans (atom (merge (beanie _tasks)
                                    (beanie _types))))
    (reset! beans-cooked? true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- set-options

  "Use reflection to invoke setters -> to set options
  on the pojo: see ref. ant#IntrospectionHelper."
  [^Project pj pojo options]

  (let [z (class pojo)
        h (IntrospectionHelper/getHelper pj z)]
    (doseq [[k v]
            (->> (cond (instance? AbstractFileSet pojo)
                       {:erroronmissingdir false}
                       (= z Delete)
                       {:includeemptydirs true}) (merge options))]
      (.setAttribute h pj pojo (name k) v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- projcomp<>

  "Configure a project component."
  {:tag ProjectComponent}

  ([pj pc options nested]
   (set-options pj pc options)
   (cfg-nested pj pc nested) pc)

  ([pj pc options]
   (projcomp<> pj pc options nil))

  ([pj pc] (projcomp<> pj pc nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- nest

  "Element is like [:fileset {:a b :c d} [[nested ...][nested ...]]].
  At the end of this function, the parent would have *added* this
  element as a child object"
  [pj par elem aggrs]

  (let [s (cs/lower-case (name (first elem)))
        dc (or (cc/get aggrs (str "addconfigured" s))
               (cc/get aggrs (str "add" s))
               (cc/get aggrs (str "create" s)))]
    (if-some [md (some-> ^MethodDescriptor dc .getMethod)]
      (let [rt (.getReturnType md)
            mn (.getName md)
            pms (.getParameterTypes md)]
        (if (cs/starts-with? mn "add")
          (let [^Class dt (cc/get _types s)
                ^Class p1 (first pms)]
            (do-with [co (if (some->> dt
                                      (.isAssignableFrom p1))
                           (ctor! dt pj) (ctor! p1 pj))]
                     (.invoke md par (into-array Object [co]))))
          (.invoke md par (make-array Object 0))))
      (trap! (str "Unknown element " (first elem))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- cfg-nested

  "*nested* typically is a vector of vectors.  Each vector models
  an xml element.  However, a special case is when nested is a
  string in which case the method addText is called."
  [pj par nested]

  (let [pz (class par)
        ;; if we find a new class, bean it and cache it
        b (cc/get @_beans pz)
        {:keys [aggrs] :as B}
        (if (nil? b)
          (do-with [m (get-bean-info pz)]
            (swap! _beans assoc pz m)) b)]
    (if (nil? B)
      (trap! (str "no bean info for " pz)))
    (cond (string? nested)
          (if-some [dc (cc/get aggrs "addtext")]
            (-> (.getMethod ^MethodDescriptor dc)
                (.invoke par (into-array Object [nested])))
            (trap! (str "wrong use of text string for " pz)))
          (or (nil? nested)
              (coll? nested))
          (doseq [p nested
                  :let [p2 (second p)
                        pc (count p)
                        p3 (nth?? p 3)
                        n (nest pj par p aggrs)]]
            ;; deal with cases where options are skipped
            (if (and (== 2 pc)
                     (not (map? p2)))
              (projcomp<> pj n nil p2)
              (projcomp<> pj n p2 p3))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- ctask<>
  ^Task [^Project p ^String tt ^String tm]
  (doto (.createTask p tt) (.setTaskName tm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- config-task

  "Reify and configure actual ant tasks."
  ^Task [^Project pj
         ^Target target
         {:keys [tname ttype options nested]}]

  (do-with
    [tk (ctask<> pj ttype tname)]
    (->> (doto tk
           (.setProject pj)
           (.setOwningTarget target))
         (.addTask target))
    (set-options pj tk options)
    (cfg-nested pj tk nested)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- proj-ant-tasks

  "Bind all the tasks to a target and a project."
  ^Target
  [^String target tasks]

  (do-with [tg (Target.)]
    (let [pj @dftprj]
      (.setName tg (or target ""))
      (.addOrReplaceTarget ^Project pj tg)
      (doseq [t tasks] (config-task pj tg t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn- run-target

  "Run ant target."
  [target tasks]

  (-> (proj-ant-tasks target tasks) exec-target))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn run-target*

  "Run an ant target."
  {:arglists '([target & tasks])}

  [^String target & tasks] (run-target target tasks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn run*

  "Run these ant tasks."
  {:arglists '([& tasks])}

  [& tasks] (run-target "" tasks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ^:private ant-task<>

  "Generate wrapper function for an ant task."
  [pj sym docstr func]

  (let [s (str func)
        tm (cs/lower-case
             (subs s (+ 1
                        (or (cs/last-index-of s ".") -1))))]
    `(defn ~sym ~docstr ;;{:no-doc true}
       ;; if not options then it could be nested
       ([~'options]
        (if-not (map? ~'options)
          (~sym nil ~'options)
          (~sym ~'options nil)))
       ([] (~sym nil nil))
       ([~'options ~'nestedElements]
        (array-map :tname ~tm
                   :ttype ~s
                   :options (or ~'options {})
                   :nested (or ~'nestedElements []))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro ^:private decl-ant-tasks

  "Introspect the default project and
  cache all registered ant tasks."
  [pj]

  (let [ts (map #(symbol %) (keys _tasks))]
    `(do ~@(map (fn [a]
                  `(ant-task<> ~pj ~a "" ~a)) ts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;this is the key process - extracting task information from
;ant.jar
(decl-ant-tasks @dftprj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn read-properties*

  "Read all ant properties."
  {:arglists '([])}
  []

  (let [f (io/file tmpdir (uid))
        ps (java.util.Properties.)]
    (run* (echoproperties
            {:failonerror false :destfile f}))
    (with-open [inp (io/input-stream f)]
      (.load ps inp))
    (clj-map ps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn clean-dir*

  "Clean an existing dir or create it."
  {:arglists '([d]
               [d options])}

  ([d]
   (clean-dir* d nil))

  ([d {:keys [quiet]
       :or {quiet true}}]
   (let [dir (io/file d)]
     (if (.exists dir)
       (run* (delete
               {:removeNotFollowedSymlinks true
                :quiet quiet}
               [[:fileset
                 {:followsymlinks false :dir dir}
                 [[:include {:name "**/*"}]]]]))
       (.mkdirs dir)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn delete-dir*

  "Remove a directory."
  {:arglists '([d]
               [d options])}

  ([d]
   (delete-dir* d nil))

  ([d {:keys [quiet]
       :or {quiet true}}]
   (let [dir (io/file d)]
     (when (.exists dir)
       (run*
         (delete
           {:removeNotFollowedSymlinks true
            :quiet quiet}
           [[:fileset {:followsymlinks false :dir dir}]]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn copy-file*

  "Copy a file to the target folder."
  {:arglists '([file toDir])}
  [file toDir]

  (.mkdirs (io/file toDir))
  (run* (copy {:file file :todir toDir})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn move-file*

  "Move a file to the target folder."
  {:arglists '([file toDir])}
  [file toDir]

  (.mkdirs (io/file toDir))
  (run* (move {:file file :todir toDir})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn delete-link*

  "Delete a file system symbolic link."
  {:arglists '([link])}
  [link]

  (run* (symlink {:action "delete" :link link})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn create-link*

  "Create a file system symbolic link."
  {:arglists '([link target]
               [link target overwrite?])}

  ([link target]
   (create-link* link target true))

  ([link target overwrite?]
   (run* (symlink {:overwrite (boolean overwrite?)
                   :action "single"
                   :link link
                   :resource target}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn disable-ant-logger*

  "Remove build logger."
  {:arglists '([])}
  []

  (if
    (-> (.getBuildListeners ^Project @dftprj)
        (.contains _ansi-logger_))
    (.removeBuildListener ^Project @dftprj _ansi-logger_)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn enable-ant-logger*

  "Add build logger."
  {:arglists '([])}
  []

  (if-not
    (-> (.getBuildListeners ^Project @dftprj)
        (.contains _ansi-logger_))
    (.addBuildListener ^Project @dftprj _ansi-logger_)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF


