;; Copyright (c) 2013-2017, Kenneth Leung. All rights reserved.
;; The use and distribution terms for this software are covered by the
;; Eclipse Public License 1.0 (http://opensource.org/licenses/eclipse-1.0.php)
;; which can be found in the file epl-v10.html at the root of this distribution.
;; By using this software in any fashion, you are agreeing to be bound by
;; the terms of this license.
;; You must not remove this notice, or any other, from this software.

(ns ^{:doc "Apache Ant project & task wrappers.
           The anatomy of an ant task is a xml construct,
           where the attributes are termed as options and
           nested elements are treated as vectors inside of
           a vector."
      :author "Kenneth Leung"}

  czlab.antclj.antlib

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
(defn uid "" ^String [] (.replaceAll (str (UID.)) "[:\\-]+" ""))
(defn ctf<> "" ^File [& [d]] (io/file (or d tmpdir) (uid)))
(defn ctd<> "" ^File [& [d]] (doto (ctf<> d) (.mkdirs)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private do-with "" [bindings & more]
  (assert (= 2 (count bindings)))
  (let [a (first bindings)
        b (last bindings)]
      `(let [~a ~b] ~@more ~a)))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(declare cfgNested)
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  skipped-tasks #{"ant" "antcall" "import" "include"
                  "copydir" "copyfile" "copypath"
                  "deltree" "execon" "javadoc2"
                  "jlink" "jspc" "mimemail"
                  "rename" "renameext" "filter"
                  "antstructure" "antversion"})

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private nth?? "" [c p] `(first (drop (dec ~p) ~c)))
(defmacro ^:private trap! "" [s] `(throw (Exception. ~s)))
(def ^:private pred-t (constantly true))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- ctor! "" [^Class cz ^Project pj]
  (let
    [c0 (try (.getConstructor cz
                              (make-array Class 0))
             (catch Throwable _))
     c1 (if (nil? c0)
          (try (.getConstructor cz
                                (into-array Class [Project]))
               (catch Throwable _)))]
     (doto
       (or (some-> ^Constructor c0
                   (.newInstance (make-array Object 0)))
           (some-> ^Constructor c1
                   (.newInstance (into-array Object [pj]))))
       (some->> (.setProjectReference pj)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;better colors that are not *dimmed*
(def
  ^:private
  _ansi-logger_
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
    (doto
      (AnsiColorLogger.)
      (.setOutputPrintStream System/out)
      (.setErrorPrintStream System/err)
      (.setMessageOutputLevel Project/MSG_INFO))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- project<>
  "" ^Project [] (doto (Project.) .init (.setName "projx")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- execTarget "" [^Target t]
  (-> (.getProject t)
      (.executeTarget (.getName t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;trick to avoid reflection warning
(defmacro ^:private gfdn
  "" [d] `(.getName ~(with-meta d {:tag 'FeatureDescriptor})))

(def ^:private create-opstrs ["addConfigured" "add" "create"])
(def ^:private create-ops (zipmap create-opstrs
                                  (mapv #(.length ^String %) create-opstrs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- cljMap ""
  ([m] (cljMap m pred-t))
  ([m pred]
   (persistent!
     (reduce
       #(let [[k v] %2]
          (if (pred k v) (assoc! %1 k v) %1)) (transient {}) m))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;create a default project.
(defonce ^:private dftprj (atom (project<>)))
(defonce ^:private beansCooked (atom false))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private _tasks
  (cljMap
    (.getTaskDefinitions ^Project @dftprj)
    (fn [k v] (not (contains? skipped-tasks k)))))
(def
  ^:private _types
  (cljMap
    (.getDataTypeDefinitions ^Project @dftprj )))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;These are the special methods for aggregating nested elements
(defn- tstSpecOps? "" [^MethodDescriptor d]
  (let
    [pms (.. d
             getMethod
             getParameterTypes)
     mn (gfdn d)
     pc (count pms)]
    (or
      (and (cs/starts-with? mn "addConfigured")
           (== 1 pc))
      (and (cs/starts-with? mn "add")
           (== 1 pc))
      (and (cs/starts-with? mn "create")
           (== 0 pc)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- getCreatorInfo "" [descs]
  (persistent!
    (reduce
      #(if (tstSpecOps? %2)
         (assoc! %1
                 (cs/lower-case (gfdn %2)) %2) %1) (transient {}) descs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- getBInfo "" [descs]
  (persistent!
    (reduce
      #(assoc! %1 (keyword (gfdn %2)) %2) (transient {}) descs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- getBeanInfo "" [^Class cz]
  (let [b (Introspector/getBeanInfo cz)]
    {:props (getBInfo (.getPropertyDescriptors b))
     ;;:ops (getBInfo (.getMethodDescriptors b))
     :aggrs (getCreatorInfo (.getMethodDescriptors b))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- beanie "" [m]
  (persistent!
    (reduce
      #(let [[_ v] %2]
         (assoc! %1 v (getBeanInfo v))) (transient {}) m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;cache bean-info of class
(if-not @beansCooked
  (do
    (def
      ^:private _beans (atom (merge (beanie _tasks)
                                    (beanie _types))))
    (reset! beansCooked true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- setOptions
  "Use reflection to invoke setters -> to set options
  on the pojo: see ref. ant#IntrospectionHelper"
  [^Project pj pojo options]
  (let [z (class pojo)
        options
        (cond
          (instance? AbstractFileSet pojo)
          (merge {:erroronmissingdir false} options)
          (= z Delete)
          (merge {:includeemptydirs true} options)
          :else options)
        h (IntrospectionHelper/getHelper pj z)]
    (doseq [[k v] options]
      (.setAttribute h pj pojo (name k) v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- projcomp<>
  "Configure a project component"
  {:tag ProjectComponent}

  ([pj pc options nested]
   (setOptions pj pc options)
   (cfgNested pj pc nested)
   pc)
  ([pj pc options]
   (projcomp<> pj pc options nil))
  ([pj pc] (projcomp<> pj pc nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- nest
  "Element is like [:fileset {:a b :c d} [[nested ...][nested ...]]].
  At the end of this function, the parent would have *added* this
  element as a child object"
  [pj par elem aggrs]

  (let [op (first elem)
        s (cs/lower-case (name op))
        dc
        (or (cc/get aggrs (str "addconfigured" s))
            (cc/get aggrs (str "add" s))
            (cc/get aggrs (str "create" s)))
        md (some-> ^MethodDescriptor dc .getMethod)
        _ (if (nil? md)
            (trap! (str "Unknown element " s)))
        rt (.getReturnType md)
        mn (.getName md)
        pms (.getParameterTypes md)]
    (if (cs/starts-with? mn "add")
      (let [^Class dt (cc/get _types s)
            ^Class p1 (first pms)]
        (do-with
          [co (if (some->> dt
                           (.isAssignableFrom p1))
                (ctor! dt pj)
                (ctor! p1 pj))]
          (.invoke md par (into-array Object [co]))))
      (.invoke md par (make-array Object 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- cfgNested
  "*nested* typically is a vector of vectors.  Each vector models
  an xml element.  However, a special case is when nested is a
  string in which case the method addText is called"
  [pj par nested]

  (let [pz (class par)
        ;; if we find a new class, bean it and cache it
        b (cc/get @_beans pz)
        b (if (nil? b)
            (do-with [m (getBeanInfo pz)]
              (swap! _beans assoc pz m)) b)
        _ (if (nil? b)
            (trap! (str "no bean info for " pz)))
        ops (:aggrs b)]
    (cond
      (string? nested)
      (if-some [dc (cc/get ops "addtext")]
        (-> (.getMethod ^MethodDescriptor dc)
            (.invoke par (into-array Object [nested])))
        (trap! (str "wrong use of text string for " pz)))
      (or (nil? nested)
          (coll? nested))
      (doseq [p nested
              :let [p2 (second p)
                    pc (count p)
                    p3 (nth?? p 3)]]
        ;; deal with cases where options are skipped
        (if (and (== 2 pc)
                 (not (map? p2)))
          (projcomp<> pj (nest pj par p ops) nil p2)
          (projcomp<> pj (nest pj par p ops) p2 p3))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- ctask<>
  "" ^Task [^Project p ^String tt ^String tm]

  (doto (.createTask p tt) (.setTaskName tm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- configTask
  "Reify and configure actual ant tasks"
  ^Task [^Project pj
         ^Target target
         {:keys [tname ttype options nested]}]

  (do-with [tk (ctask<> pj ttype tname)]
    (->> (doto tk
           (.setProject pj)
           (.setOwningTarget target))
         (.addTask target))
    (setOptions pj tk options)
    (cfgNested pj tk nested)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- projAntTasks
  "Bind all the tasks to a target and a project"
  ^Target
  [^String target tasks]

  (do-with [tg (Target.)]
    (let [pj @dftprj]
      (.setName tg (or target ""))
      (.addOrReplaceTarget ^Project pj tg)
      (doseq [t tasks]
        (configTask pj tg t)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- projAntTasks*
  "Bind all the tasks to a target and a project"
  ^Target
  [target & tasks]
  (projAntTasks target tasks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- runTarget
  "Run ant target"
  [target tasks]
  (-> (projAntTasks target tasks) execTarget))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn runTarget*
  "Run ant target" [^String target & tasks] (runTarget target tasks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn run* "Run ant tasks" [& tasks] (runTarget "" tasks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private antTask<>
  "Generate wrapper function for an ant task"
  [pj sym docstr func]

  (let [s (str func)
        tm (cs/lower-case
             (.substring s
                         (inc (.lastIndexOf s "."))))]
    `(defn ~sym ~docstr ;;{:no-doc true}
       ;; if not options then it could be nested
       ([~'options]
        (if-not (map? ~'options)
          (~sym nil ~'options)
          (~sym ~'options nil)))
       ([] (~sym nil nil))
       ([~'options ~'nestedElements]
        (doto
          {:tname ~tm
           :ttype ~s
           :options (or ~'options {})
           :nested (or ~'nestedElements [])})))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private declAntTasks
  "Introspect the default project and cache all registered ant tasks"
  [pj]
  (let [ts (mapv #(symbol %) (keys _tasks))]
    `(do ~@(map (fn [a]
                  `(antTask<> ~pj ~a "" ~a)) ts))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(declAntTasks @dftprj)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn readProperties
  "Read all ant properties" ^APersistentMap []

  (let [ps (java.util.Properties.)
        f (ctf<>)
        _ (run*
            (echoproperties
              {:failonerror false
               :destfile f}))]
    (with-open [inp (io/input-stream f)] (.load ps inp))
    (cljMap ps)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn cleanDir
  "Clean an existing dir or create it"

  ([d] (cleanDir d nil))
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
;;
(defn deleteDir
  "Remove a directory"

  ([d] (deleteDir d nil))
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
;;
(defn copyFile
  "Copy a file to the target folder"
  [file toDir]

  (.mkdirs (io/file toDir))
  (run*
    (copy {:file file :todir toDir})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn moveFile
  "Move a file to the target folder"
  [file toDir]

  (.mkdirs (io/file toDir))
  (run*
    (move {:file file :todir toDir})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deleteLink
  "Delete a file system symbolic link"
  [link]
  (run* (symlink {:action "delete" :link link})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn createLink
  "Create a file system symbolic link"

  ([link target] (createLink link target true))
  ([link target overwrite?]
   (run*
     (symlink {:overwrite (boolean overwrite?)
               :action "single"
               :link link
               :resource target}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn disableAntLogger "Remove build logger" []
  (if
    (-> (.getBuildListeners ^Project @dftprj)
        (.contains _ansi-logger_))
    (.removeBuildListener ^Project @dftprj _ansi-logger_)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn enableAntLogger "Add build logger" []
  (if-not
    (-> (.getBuildListeners ^Project @dftprj)
        (.contains _ansi-logger_))
    (.addBuildListener ^Project @dftprj _ansi-logger_)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

