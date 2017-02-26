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
           nested elements are treated as vectors of
           vectors or maps."
      :author "Kenneth Leung"}

  czlab.antclj.antlib

  (:import [org.apache.tools.ant.taskdefs.optional.unix Symlink]
           [java.beans
            FeatureDescriptor
            MethodDescriptor
            Introspector
            PropertyDescriptor]
           [java.lang.reflect Method]
           [java.util Map]
           [java.io File]
           [org.apache.tools.ant.taskdefs
            Javadoc
            Java
            Copy
            Chmod
            Concat
            Move
            Mkdir
            Tar
            Replace
            ExecuteOn
            Delete
            Jar
            Zip
            ExecTask
            Javac
            Javadoc$AccessType
            Replace$Replacefilter
            Replace$NestedString
            Tar$TarFileSet
            Tar$TarCompressionMethod
            Javac$ImplementationSpecificArgument]
           [org.apache.tools.ant.listener
            AnsiColorLogger
            TimestampedLogger]
           [org.apache.tools.ant.types
            Commandline$Argument
            Commandline$Marker
            PatternSet$NameEntry
            Environment$Variable
            FileList$FileName
            FileList
            AbstractFileSet
            ZipFileSet
            Reference
            Mapper
            FileSet
            Path
            DirSet]
           [org.apache.tools.ant
            IntrospectionHelper
            ProjectComponent
            NoBannerLogger
            Project
            Target
            Task]
           [org.apache.tools.ant.taskdefs.optional.junit
            FormatterElement$TypeAttribute
            JUnitTask$SummaryAttribute
            JUnitTask$ForkMode
            JUnitTask
            JUnitTest
            BatchTest
            FormatterElement]
           [java.rmi.server UID]
           [clojure.lang APersistentMap]
           [org.apache.tools.ant.util
            FileNameMapper
            ChainedMapper
            GlobPatternMapper])

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

(declare maybeCfgNested)
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
(defn- ctor! "" [cz pj]
  (let
    [c0 (try (. cz
                getConstructor
                (make-array Class 0))
             (catch Throwable _))
     c1 (if (nil? c0)
          (try (. cz
                  getConstructor
                  (into-array Class [Project]))
               (catch Throwable _)))
     r0
     (some-> c0
             (.newInstance (make-array Object 0)))
     r1
     (some-> c1
             (.newInstance (into-array Object [pj])))
     rc (or r0 r1)]
    (some->> rc (.setProjectReference pj))
    rc))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  ansiLogger
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
;;
(defmacro ^:private gfdn
  "" [d] `(.getName ~(with-meta d {:tag 'FeatureDescriptor})))

(def ^:private create-opstrs ["addConfigured" "add" "create"])
(def ^:private create-ops (zipmap create-opstrs
                                  (mapv #(.length %) create-opstrs)))

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
    (. ^Project
       @dftprj getTaskDefinitions)
    (fn [k v] (not (contains? skipped-tasks k)))))
(def
  ^:private _types
  (cljMap
    (. ^Project
       @dftprj getDataTypeDefinitions)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- tstSpecOps? "" [^MethodDescriptor d]
  (let
    [mtd (.getMethod d)
     mn (gfdn d)
     pms (.getParameterTypes mtd)
     pc (count pms)
     rt (.getReturnType mtd)]
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
     :ops (getBInfo (.getMethodDescriptors b))
     :aggrs (getCreatorInfo (.getMethodDescriptors b))}))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- beanie "" [m]
  (persistent!
    (reduce
      #(let [[k v] %2]
         (assoc! %1 v (getBeanInfo v))) (transient {}) m)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;cache ant task names as symbols, and cache bean-info of class
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
  on the pojo"
  [^Project pj pojo options]
  (let [h (IntrospectionHelper/getHelper pj
                                         (class pojo))
        z (class pojo)
        options
        (cond
          (instance? AbstractFileSet pojo)
          (merge {:erroronmissingdir false} options)
          (= z Delete)
          (merge {:includeemptydirs true} options)
          :else options)]
    (doseq [[k v] options]
      (. h setAttribute pj pojo (name k) v))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- projcomp<>
  "Configure a project component"
  {:tag ProjectComponent}

  ([^Project pj ^ProjectComponent pc options nested]
   (setOptions pj pc options)
   (maybeCfgNested pj pc nested)
   pc)
  ([pj pc options]
   (projcomp<> pj pc options nil))
  ([pj pc] (projcomp<> pj pc nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- nest "" [pj par elem aggrs]
  (let [op (first elem)
        s (cs/lower-case (name op))
        dc
        (or (cc/get aggrs (str "addconfigured" s))
            (cc/get aggrs (str "add" s))
            (cc/get aggrs (str "create" s)))
        md (some-> dc .getMethod)
        _ (when (nil? md)
            (trap! (str "Unknown element " s)))
        rt (.getReturnType md)
        mn (.getName md)
        pms (.getParameterTypes md)]
    (if (cs/starts-with? mn "add")
      (let
        [dt (cc/get _types s)
         p1 (first pms)
         co
         (if (and (some? dt)
                  (.isAssignableFrom p1 dt))
           (ctor! dt pj)
           (ctor! p1 pj))]
        (.invoke md par (into-array Object [co]))
        co)
      (.invoke md par (make-array Object 0)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeCfgNested "" [pj par nested]
  (let [pz (class par)
        b (if-some [m (cc/get @_beans pz)]
            m
            (let [m (getBeanInfo pz)]
              (swap! _beans assoc pz m) m))
        ops (:aggrs b)]
    (if (nil? b)
      (trap! (str "no bean info for class " pz)))
    (cond
      (string? nested)
      (if (contains? ops "addtext")
        (.addText par nested)
        (trap! (str "incorrect use of text string for " pz)))
      :else
      (doseq [p nested
              :let [p2 (second p)
                    pc (count p)
                    p3 (nth?? p 3)]]
        ;; deal with cases where options are skipped
        (if (and (== 2 pc)
                 (not (map? p2)))
          (projcomp<> pj (nest pj par p ops) nil p2)
          (projcomp<> pj (nest pj par p ops) p2 p3))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- configTask
  "Reify and configure actual ant tasks"
  ^Task [^Project pj
         ^Target target
         {:keys [tname task options nested]}]

  (let []
    (->> (doto ^Task
           task
           (.setProject pj)
           (.setOwningTarget target))
         (.addTask target))
    (setOptions pj task options)
    (maybeCfgNested pj task nested)
    task))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- projAntTasks
  "Bind all the tasks to a target and a project"
  ^Target
  [^String target tasks]

  (let [pj @dftprj
        tg (Target.)]
    (. tg setName (or target ""))
    (. ^Project pj addOrReplaceTarget tg)
    ;;(println (str "number of tasks ==== " (count tasks)))
    (doseq [t tasks]
      (configTask pj tg t))
    tg))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- projAntTasks*
  "Bind all the tasks to a target and a project"
  ^Target
  [target & tasks]
  (projAntTasks target tasks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn runTarget
  "Run ant target"
  [target tasks]
  (-> (projAntTasks target tasks) execTarget))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn runTarget*
  "Run ant target" [target & tasks] (runTarget target tasks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn runTasks "Run ant tasks" [tasks] (runTarget "" tasks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn runTasks* "Run ant tasks" [& tasks] (runTarget "" tasks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn run* "Run ant tasks" [& tasks] (runTarget "" tasks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn run "Run ant tasks" [tasks] (runTarget "" tasks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- ctask<>
  "" ^Task [^Project p ^String tt ^String tm]

  (doto (.createTask p tt) (.setTaskName tm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private antTask<>
  "Generate wrapper function for an ant task"
  ([pj sym docstr func preopt]
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
        ([~'options  ~'nestedElements]
         (let [tk# (ctask<> ~pj ~s ~tm)
               o# (or ~'options {})
               n# (or ~'nestedElements [])
               r# {:pre-options ~preopt
                   :tname ~tm
                   :task tk#
                   :options o#
                   :nested n#}]
             r#)))))
  ([pj sym docstr func]
   `(antTask<> ~pj ~sym ~docstr ~func nil)))

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
    (-> (. ^Project @dftprj getBuildListeners)
        (.contains ansiLogger))
    (. ^Project @dftprj removeBuildListener ansiLogger)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn enableAntLogger "Add build logger" []
  (if-not
    (-> (. ^Project @dftprj getBuildListeners)
        (.contains ansiLogger))
    (. ^Project @dftprj addBuildListener ansiLogger)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

