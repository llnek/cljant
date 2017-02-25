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
(declare maybeCfgNested)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  okay-tasks
  #{;;"ant"
    ;;"antcall"
    ;;"antstructure"
    ;;"antversion"
    "apply"
    "attrib"
    ;;"attributenamespacedef"
    ;;"augment"
    ;;"available"
    "basename"
    ;;"bindtargets"
    ;;"blgenclient"
    ;;"buildnumber"
    ;;"bunzip2"
    ;;"bzip2"
    "cab"
    ;;"cccheckin"
    ;;"cccheckout"
    ;;"cclock"
    ;;"ccmcheckin"
    ;;"ccmcheckintask"
    ;;"ccmcheckout"
    ;;"ccmcreatetask"
    ;;"ccmkattr"
    ;;"ccmkbl"
    ;;"ccmkdir"
    ;;"ccmkelem"
    ;;"ccmklabel"
    ;;"ccmklbtype"
    ;;"ccmreconfigure"
    ;;"ccrmtype"
    ;;"ccuncheckout"
    ;;"ccunlock"
    ;;"ccupdate"
    "checksum"
    "chgrp"
    "chmod"
    "chown"
    ;;"classloader"
    ;;"commandlauncher"
    ;;"componentdef"
    "concat"
    ;;"condition"
    "copy"
    ;;"copydir"
    ;;"copyfile"
    ;;"copypath"
    ;;"cvs"
    ;;"cvschangelog"
    ;;"cvspass"
    ;;"cvstagdiff"
    ;;"cvsversion"
    ;;"defaultexcludes"
    "delete"
    ;;"deltree"
    ;;"depend"
    ;;"dependset"
    ;;"diagnostics"
    ;;"dirname"
    "ear"
    "echo"
    "echoproperties"
    ;;"echoxml"
    ;;"ejbjar"
    "exec"
    ;;"execon"
    ;;"fail"
    ;;"filter"
    "fixcrlf"
    "genkey"
    "get"
    "gunzip"
    "gzip"
    "hostinfo"
    ;;"import"
    ;;"include"
    "input"
    ;;"iplanet-ejbc"
    "jar"
    ;;"jarlib-available"
    ;;"jarlib-display"
    ;;"jarlib-manifest"
    ;;"jarlib-resolve"
    "java"
    "javac"
    "javacc"
    "javadoc"
    ;;"javadoc2"
    "javah"
    "jjdoc"
    "jjtree"
    ;;"jlink"
    ;;"jspc"
    "junit"
    "junitreport"
    "length"
    ;;"loadfile"
    ;;"loadproperties"
    ;;"loadresource"
    ;;"local"
    ;;"macrodef"
    "mail"
    ;;"makeurl"
    "manifest"
    ;;"manifestclasspath"
    ;;"mimemail"
    "mkdir"
    "move"
    ;;"native2ascii"
    ;;"nice"
    ;;"parallel"
    "patch"
    ;;"pathconvert"
    ;;"presetdef"
    ;;"projecthelper"
    "property"
    ;;"propertyfile"
    ;;"propertyhelper"
    ;;"pvcs"
    ;;"record"
    ;;"rename"
    ;;"renameext"
    "replace"
    "replaceregexp"
    ;;"resourcecount"
    ;;"retry"
    ;;"rmic"
    "rpm"
    ;;"schemavalidate"
    ;;"script"
    ;;"scriptdef"
    ;;"sequential"
    ;;"serverdeploy"
    "setpermissions"
    "setproxy"
    "signjar"
    "sleep"
    ;;"soscheckin"
    ;;"soscheckout"
    ;;"sosget"
    ;;"soslabel"
    "sql"
    "style"
    ;;"subant"
    "symlink"
    "sync"
    "tar"
    ;;"taskdef"
    "tempfile"
    "touch"
    ;;"translate"
    "truncate"
    "tstamp"
    ;;"typedef"
    "unjar"
    "untar"
    "unwar"
    "unzip"
    ;;"uptodate"
    "verifyjar"
    ;;"vssadd"
    ;;"vsscheckin"
    ;;"vsscheckout"
    ;;"vsscp"
    ;;"vsscreate"
    ;;"vssget"
    ;;"vsshistory"
    ;;"vsslabel"
    ;;"waitfor"
    "war"
    "whichresource"
    ;;"wljspc"
    ;;"xmlproperty"
    ;;"xmlvalidate"
    "xslt"
    "zip"})

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
(defn- capstr
  "Capitalize the 1st character"
  ^String [s] (if s (cs/capitalize (name s)) ""))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private ansi-colors
  (cs/join "\n"
           ["AnsiColorLogger.ERROR_COLOR=0;31"
            "AnsiColorLogger.WARNING_COLOR=0;35"
            "AnsiColorLogger.INFO_COLOR=0;36"
            "AnsiColorLogger.VERBOSE_COLOR=0;32"
            "AnsiColorLogger.DEBUG_COLOR=0;34"]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- hackAnsiColors "" []
  (let [f (-> (System/getProperty "java.io.tmpdir")
              (io/file "czlab-antlogansi.colors"))]
    (if-not (.exists f) (spit f ansi-colors))
    (System/setProperty "ant.logger.defaults"
                        (.getCanonicalPath f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- project<>
  "New project" ^Project []

  (let [_ (hackAnsiColors)
        lg (doto
             (AnsiColorLogger.)
             (.setOutputPrintStream System/out)
             (.setErrorPrintStream System/err)
             (.setMessageOutputLevel Project/MSG_INFO))]
    (doto (Project.)
      .init
      (.setName "projx")
      (.addBuildListener lg))))

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
    (fn [k v] (contains? okay-tasks k))))
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
      ^:private _beans (merge (beanie _tasks)
                              (beanie _types)))
    (reset! beansCooked true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(def
  ^:private
  setterArgTypes
  ;;add more types when needed
  [String
   java.io.File
   Boolean/TYPE
   Boolean
   Integer/TYPE
   Integer
   Long/TYPE
   Long
   org.apache.tools.ant.types.Path])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- method?
  "Find this setter method via best match,
   if found, returns a tuple [method classofarg]"
  [^Class cz ^String m]

  (let [arr (make-array java.lang.Class 1)]
    (some
      (fn [^Class z]
        (try
          (aset #^"[Ljava.lang.Class;" arr 0 z)
          [(.getMethod cz m arr) z]
          (catch Throwable _))) setterArgTypes)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmulti ^:private koerce "Converter" (fn [_ a b] [a (class b)]))

(defmethod koerce [Integer/TYPE String] [_ _ ^String v] (Integer/parseInt v (int 10)))
(defmethod koerce [Integer String] [_ _ ^String v] (Integer/parseInt v (int 10)))
(defmethod koerce [Integer/TYPE Long] [_ _ ^Long v] (.intValue v))

(defmethod koerce [Integer Long] [_ _ ^Long v] (.intValue v))
(defmethod koerce [Integer/TYPE Integer] [_ _ ^Integer v] v)
(defmethod koerce [Integer Integer] [_ _ ^Integer v] v)

(defmethod koerce [Long/TYPE String] [_ _ ^String v] (Long/parseLong v (int 10)))
(defmethod koerce [Long String] [_ _ ^String v] (Long/parseLong v (int 10)))

(defmethod koerce [Long/TYPE Long] [_ _ ^Long v] v)
(defmethod koerce [Long Long] [_ _ ^Long v] v)

(defmethod koerce [Path File] [^Project pj _ ^File v] (Path. pj (.getCanonicalPath v)))
(defmethod koerce [Path String] [^Project pj _ ^String v] (Path. pj v))

(defmethod koerce [File String] [_ _ ^String v] (io/file v))
(defmethod koerce [File File] [_ _ v] v)

(defmethod koerce :default [_ pz _] (Exception. (str "expected class " pz)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- coerce
  "Best attempt to convert a given value"
  [pj pz value]
  (cond
    (or (= Boolean/TYPE pz)
        (= Boolean pz))
    (= "true" (str value))

    (= String pz)
    (str value)

    :else
    (koerce pj pz value)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- setProp!
  "" [wm pojo k arr]

  (try
    (. ^Method wm invoke pojo arr)
  (catch Throwable _
    (println (str "failed to set " k " for " (class pojo)))
    (throw _))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- setOptionsXXX
  "Use reflection to invoke setters -> to set options
  on the pojo"

  ([pj pojo options]
   (setOptionsXXX pj pojo options nil))

  ([^Project pj pojo options skips]
   (let [arr (object-array 1)
         cz (class pojo)
         ps (:props (cc/get _beans cz))]
     (if (instance? ProjectComponent pojo)
       (. ^ProjectComponent pojo setProject pj))
     (doseq [[k v] options
             :when (not (contains? skips k))]
       (if-some [^PropertyDescriptor
                 pd (cc/get ps k)]
         (->
           ;;some cases the beaninfo is erroneous
           ;;so fall back to use *best-try*
           (let [mn (str "set" (capstr k))
                 wm (.getWriteMethod pd)
                 pt (.getPropertyType pd)]
             (if (some? wm)
               (do (->> (coerce pj pt v)
                        (aset arr 0)) wm)
               (let [[wm pt]
                     (method? cz mn)]
                 (if (nil? wm)
                   (trap! (str mn " not in " cz)))
                 (->> (coerce pj pt v)
                      (aset arr 0))
                 wm)))
           (setProp! pojo k arr))
         (trap! (str "prop['" k "'] not in " cz)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- setOptions
  "Use reflection to invoke setters -> to set options
  on the pojo"

  ([pj pojo options] (setOptions pj pojo options nil))

  ([^Project pj pojo options skips]
   (let [h (IntrospectionHelper/getHelper pj
                                          (class pojo))]
     (doseq [[k v] options]
       (. h setAttribute pj pojo (name k) v)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- projcomp<>
  "Configure a project component"
  {:tag ProjectComponent}

  ([^Project pj ^ProjectComponent pc options nested]
   (if (fn? options)
     (options pj pc)
     (setOptions pj pc options))
   (if (fn? nested)
     (nested pj pc)
     (maybeCfgNested pj pc nested))
   pc)
  ([pj pc options]
   (projcomp<> pj pc options nil))
  ([pj pc] (projcomp<> pj pc nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private antFileSet
  "" [options] `(merge {:erroronmissingdir false} ~options))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- antChainedMapper
  "Handles glob only"
  ^ProjectComponent [nested pj cm]

  (doseq [n nested]
    (case (:type n)
      :glob
      (->> (doto (GlobPatternMapper.)
             (.setFrom (:from n))
             (.setTo (:to n)))
           (. ^ChainedMapper cm add ))
      (trap! (str "unknown mapper: " n))))
  cm)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- antFormatter ""
  ^ProjectComponent [options pj tk]

  (if-some
    [[k v] (find options :type)]
    (. ^FormatterElement tk
       setType
       (doto (FormatterElement$TypeAttribute.)
         (.setValue (str v)))))
  (setOptions pj tk options #{:type})
  tk)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- setClassPath
  "Build a nested Path structure for classpath"
  [^Project pj ^Path root paths]

  (doseq [p paths]
    (case (first p)
      :location
      (doto (.createPath root)
        (.setLocation (io/file (str (last p)))))
      :refid
      (trap! "path:refid not supported")
      ;;(doto (.createPath root) (.setRefid (last p)))
      :fileset
      (->> (projcomp<>
             pj
             (FileSet.)
             (antFileSet (second p))
             (nth?? p 3))
           (.addFileset root))
      (trap! (str "unknown path: " p))))
  root)

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
        _ (if (nil? md)
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
  (let [b (cc/get _beans (class par))
        ops (:aggrs b)]
    (doseq [p nested]
      (projcomp<> pj
                  (nest pj par p ops)
                  (second p)
                  (nth?? p 3)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- XXmaybeCfgNested "" [pj tk nested]
  (doseq [p nested]
    (case (first p)

      :compilerarg
      (if-some [n (:line (last p))]
        (-> (.createCompilerArg tk)
            (.setLine ^String n)))

      :file
      (let [n (FileList$FileName.)]
        (. n setName (str (:name (second p))))
        (. tk addConfiguredFile n))

      :classpath
      (setClassPath pj
                    (.createClasspath tk) (last p))

      :sysprops
      (doseq [[k v] (last p)]
        (->> (doto (Environment$Variable.)
                   (.setKey (name k))
                   (.setValue (str v)))
             (.addSysproperty tk)))

      :formatter
      (->> (projcomp<>
             pj
             (FormatterElement.)
             (partial antFormatter (last p)))
           (.addFormatter tk))

      :include
      (let [v (cs/trim (str (last p)))]
        (if-not (empty? v)
          (-> (.createInclude tk)
              (.setName v))))

      :exclude
      (let [v (cs/trim (str (last p)))]
        (if-not (empty? v)
          (-> (.createExclude tk)
              (.setName v))))

      :filelist
      (->> (projcomp<> pj (FileList.) (second p) (nth?? p 3))
           (.addFilelist tk))

      :patternset
      (projcomp<> pj
                  (.createPatternSet tk) (second p) (nth?? p 3))

      :dirset
      (->> (projcomp<> pj
                       (DirSet.)
                       (antFileSet (second p)) (nth?? p 3))
           (.addDirset tk ))

      :fileset
      (let [s (projcomp<> pj
                          (FileSet.)
                          (antFileSet (second p)) (nth?? p 3))]
        (if (instance? BatchTest tk)
          (.addFileSet tk s)
          (.addFileset tk s)))

      :argvalues
      (doseq [v (last p)]
        (-> (.createArg tk)
            (.setValue (str v))))

      :argpaths
      (doseq [v (last p)]
        (-> (.createArg tk)
            (.setPath (Path. pj (str v)))))

      :arglines
      (doseq [v (last p)]
        (-> (.createArg tk)
            (.setLine (str v))))

      :replacefilter
      (doto (.createReplacefilter tk)
            (.setToken (:token (nth p 1)))
            (.setValue (:value (nth p 1))))

      :replacevalue
      (-> (.createReplaceValue tk)
          (.addText (:text (last p))))

      :replacetoken
      (-> (.createReplaceToken tk)
          (.addText (:text (last p))))

      :test
      (. tk addTest (projcomp<> pj
                                (JUnitTest.)
                                (second p) (nth?? p 3)))

      :chainedmapper
      (. tk add (projcomp<> pj
                            (ChainedMapper.)
                            (second p)
                            (partial antChainedMapper (nth?? p 3))))

      :targetfile
      (.createTargetfile tk)

      :srcfile
      (.createSrcfile tk)

      :batchtest
      (projcomp<> pj
                  (.createBatchTest tk) (second p) (nth?? p 3))

      :tarfileset
      (projcomp<> pj
                  (.createTarFileSet tk) (second p) (nth?? p 3))

      :zipfileset
      (projcomp<> pj
                  (let [z (ZipFileSet.)]
                    (. ^Zip tk addZipfileset z) z)
                  (second p)
                  (nth?? p 3))

      (trap! (str "unknown nested: " p)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- xxx-preopts "" [tk options] [options #{}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- delete-pre-opts
  "" [tk options] [(merge {:includeEmptyDirs true} options) #{}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- junit-preopts
  "" [^JUnitTask tk options]

  (if-some [v (:printsummary options)]
    (. tk
       setPrintsummary
       (doto
         (JUnitTask$SummaryAttribute.)
         (.setValue (str v)))))

  (if-some [v (:forkMode options)]
    (. tk
       setForkMode
       (doto
         (JUnitTask$ForkMode.)
         (.setValue (str v)))))

  [options #{:printsummary :forkMode}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- jdoc-preopts "" [tk options]
  (if-some
    [v (:access options)]
    (. ^Javadoc tk
       setAccess
       (doto
         (Javadoc$AccessType.)
         (.setValue (str v)))))
  [options #{:access}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- tar-preopts "" [tk options]
  (if-some
    [v (:compression options)]
    (. ^Tar tk
       setCompression
       (doto
         (Tar$TarCompressionMethod.)
         (.setValue (str v)))))
  [options #{:compression}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- configTask
  "Reify and configure actual ant tasks"
  ^Task [^Project pj
         ^Target target
         {:keys [pre-options
                 tname task options nested]}]

  (let [preopts (or pre-options
                    xxx-preopts)]
    (->> (doto ^Task
           task
           (.setProject pj)
           (.setOwningTarget target))
         (.addTask target))
    (->> (preopts task options)
         (cc/apply setOptions pj task))
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
  "Run ant tasks"
  [target tasks]
  (-> (projAntTasks target tasks) execTarget))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn runTarget*
  "Run ant tasks" [target & tasks] (runTarget target tasks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn runTasks
  "Run ant tasks"
  [tasks] (runTarget "" tasks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn runTasks* "Run ant tasks" [& tasks] (runTarget "" tasks))

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
        ([~'options] (~sym ~'options nil))
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
           (if (nil? (:pre-options r#))
             (->> (case ~s
                    ;;certain classes need special handling of properties
                    ;;due to type mismatch or property name
                    ;;inconsistencies
                    "delete" delete-pre-opts
                    "junit" junit-preopts
                    "javadoc" jdoc-preopts
                    "tar" tar-preopts
                    nil)
                  (assoc r# :pre-options))
             r#))))))
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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn cleanDir
  "Clean an existing dir or create it"

  ([d] (cleanDir d nil))
  ([d {:keys [quiet]
       :or {quiet true}}]
   (let [dir (io/file d)]
     (if (.exists dir)
       (runTasks* (delete
                    {:removeNotFollowedSymlinks true
                     :quiet quiet}
                    [[:fileset
                      {:followSymlinks false :dir dir}
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
       (runTasks*
         (delete
           {:removeNotFollowedSymlinks true
            :quiet quiet}
           [[:fileset {:followSymlinks false :dir dir}]]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn copyFile
  "Copy a file to the target folder"
  [file toDir]

  (.mkdirs (io/file toDir))
  (runTasks*
    (copy {:file file :todir toDir})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn moveFile
  "Move a file to the target folder"
  [file toDir]

  (.mkdirs (io/file toDir))
  (runTasks*
    (move {:file file :todir toDir})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn deleteLink
  "Delete a file system symbolic link"
  [link]
  (runTasks* (symlink {:action "delete" :link link})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn createLink
  "Create a file system symbolic link"

  ([link target] (createLink link target true))
  ([link target overwrite?]
   (runTasks*
     (symlink {:overwrite (boolean overwrite?)
               :action "single"
               :link link
               :resource target}))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

