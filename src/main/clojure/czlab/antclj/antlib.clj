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
;; Copyright (c) 2013-2016, Kenneth Leung. All rights reserved.

(ns ^{:doc "Apache Ant project & task wrappers.
           The anatomy of an ant task is a xml construct,
           where the attributes are termed as options and
           nested elements are treated as vectors of
           vectors or maps."
      :author "Kenneth Leung"}

  czlab.antclj.antlib

  (:import [org.apache.tools.ant.taskdefs.optional.unix Symlink]
           [java.beans Introspector PropertyDescriptor]
           [java.lang.reflect Method]
           [java.util Stack]
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
            Javac]
           [org.apache.tools.ant.listener
            AnsiColorLogger
            TimestampedLogger]
           [org.apache.tools.ant.types
            Commandline$Argument
            Commandline$Marker
            PatternSet$NameEntry
            Environment$Variable
            Reference
            Mapper
            FileSet
            Path
            DirSet]
           [org.apache.tools.ant
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
            GlobPatternMapper]
           [org.apache.tools.ant.taskdefs
            Javadoc$AccessType
            Replace$Replacefilter
            Replace$NestedString
            Tar$TarFileSet
            Tar$TarCompressionMethod
            Javac$ImplementationSpecificArgument])

  (:require [clojure.java.io :as io]
            [clojure.string :as cs]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;(set! *warn-on-reflection* true)
(declare maybeCfgNested)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private trap! "" [s] `(throw (Exception. ~s)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private fopt "" [o t] `(find ~o ~t))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- capstr
  "Capitalize the 1st character"
  ^String
  [^String s]
  (if (and (some? s)
           (not (.isEmpty s)))
    (str (.toUpperCase (.substring s 0 1))
         (.substring s 1))
    s))

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
(defn- hackAnsiColors
  ""
  []
  (let [tmp (System/getProperty "java.io.tmpdir")
        f (io/file tmp "czlab-antlogansi.colors")]
    (if-not (.exists f)
      (spit f ansi-colors :encoding "utf-8"))
    (System/setProperty "ant.logger.defaults"
                        (.getCanonicalPath f))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- project<>
  "Create a new ant project"
  ^Project
  []
  (hackAnsiColors)
  (let [lg (doto
             (AnsiColorLogger.)
             (.setOutputPrintStream System/out)
             (.setErrorPrintStream System/err)
             (.setMessageOutputLevel Project/MSG_INFO))]
    (doto (Project.)
      (.init)
      (.setName "projx")
      (.addBuildListener lg))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- execTarget
  "Run and execute a target"
  [^Target t]
  (-> (.getProject t)
      (.executeTarget (.getName t))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private gpdn
  ""
  [pd]
  `(.getName ~(with-meta pd {:tag 'PropertyDescriptor})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- getBeanInfo
  "Get java bean info of this class"
  [cz]
  (persistent!
    (reduce
      #(assoc! %1 (keyword (gpdn %2)) %2)
      (transient {})
      (-> (Introspector/getBeanInfo cz)
          (.getPropertyDescriptors)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;create a default project.
(defonce ^:private dftprj (atom (project<>)))
(defonce ^:private beansCooked (atom false))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;cache ant task names as symbols, and cache bean-info of class
(if-not @beansCooked
  (let [beans (atom {})
        syms (atom [])]
    (doseq [[k v] (.getTaskDefinitions ^Project @dftprj)]
      (when (.isAssignableFrom Task v)
        (swap! syms
               conj
               (str "ant" (capstr k)) k)
        (swap! beans assoc v (getBeanInfo v))))
    (def ^:private _tasks (atom (partition 2 (map #(symbol %) @syms))))
    (def ^:private _props (atom @beans))
    (reset! beansCooked true)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeProps
  "Add bean info for non-task classes"
  [cz]
  (let [b (getBeanInfo cz)] (swap! _props assoc cz b) b))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- method?
  "Find this setter method via best match,
   if found, returns a tuple [method classofarg]"
  [^Class cz ^String m]
  (let [arr (make-array java.lang.Class 1)]
    (some
      (fn [^Class z]
        (aset #^"[Ljava.lang.Class;" arr 0 z)
        (try
          [(.getMethod cz m arr) z]
          (catch Throwable _)))
      ;;add more types when needed
      [String
       java.io.File
       Boolean/TYPE
       Boolean
       Integer/TYPE
       Integer
       Long/TYPE
       Long
       org.apache.tools.ant.types.Path])))

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
  ""
  [^Method wm pojo k arr]
  (try
    (.invoke wm pojo arr)
  (catch Throwable e#
    (println (str "failed to set " k " for " (class pojo)))
    (throw e#))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- setOptions
  "Use reflection and invoke setters to set options
  on the pojo"
  ([pj pojo options]
   (setOptions pj pojo options nil))
  ([pj pojo options skips]
   (let [arr (object-array 1)
         cz (class pojo)
         ps (or (get @_props cz)
                (maybeProps cz))]
     (doseq [[k v] options
             :when (not (contains? skips k))]
       (if-some [^PropertyDescriptor pd (get ps k)]
         (->
           ;;some cases the beaninfo is erroneous
           ;;so fall back to use *best-try*
           (let [mn (str "set" (capstr (name k)))
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
(defn- antTarFileSet
  "Configure a TarFileSet Object"
  {:tag Tar$TarFileSet}
  ([^Project pj ^Tar$TarFileSet fs options nested]
   (setOptions pj fs options)
   (.setProject fs pj)
   (maybeCfgNested pj fs nested)
   fs)
  ([pj fs options]
   (antTarFileSet pj fs options nil))
  ([pj fs] (antTarFileSet pj fs nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- antFileSet
  "Create a FileSet Object"
  {:tag FileSet}
  ([pj options]
   (antFileSet pj options nil))
  ([pj]
   (antFileSet pj nil nil))
  ([^Project pj options nested]
   (let [fs (FileSet.)]
     (setOptions pj
                 fs
                 (-> {:errorOnMissingDir false}
                     (merge options)))
     (.setProject fs pj)
     (maybeCfgNested pj fs nested)
     fs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- antBatchTest
  "Configure a BatchTest Object"
  {:tag BatchTest }
  ([^Project pj ^BatchTest bt options nested]
   (setOptions pj bt options)
   (maybeCfgNested pj bt nested)
   bt)
  ([pj bt options]
   (antBatchTest pj bt options nil))
  ([pj bt]
   (antBatchTest pj bt nil nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- antJunitTest
  "Configure a single JUnit Test Object"
  {:tag JUnitTask }
  ([pj] (antJunitTest pj nil nil))
  ([pj options]
   (antJunitTest pj options nil))
  ([^Project pj options nested]
   (let [jt (JUnitTest.)]
     (setOptions pj jt options)
     (maybeCfgNested pj jt nested)
     jt)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- antChainedMapper
  "Handles glob only"
  {:tag FileNameMapper}
  ([pj options]
   (antChainedMapper pj options nil))
  ([pj]
   (antChainedMapper pj nil nil))
  ([^Project pj options nested]
   (let [cm (ChainedMapper.)]
     (setOptions pj cm options)
     (doseq [n nested]
       (case (:type n)
         :glob
         (->> (doto (GlobPatternMapper.)
                (.setFrom (:from n))
                (.setTo (:to n)))
              (.add cm))
         nil))
     cm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- fmtr-preopts
  ""
  [^FormatterElement tk options]
  (if-some [[k v] (find options :type)]
    (.setType tk
              (doto (FormatterElement$TypeAttribute.)
                (.setValue (str v)))))
  [options #{:type}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- antFormatter
  "Create a Formatter Object"
  {:tag FormatterElement }
  ([pj options] (antFormatter pj options nil))
  ([pj] (antFormatter pj nil nil))
  ([^Project pj options nested]
   (let [fe (FormatterElement.)]
     (apply setOptions pj fe (fmtr-preopts fe options))
     (.setProject fe pj)
     fe)))

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
      (->> (antFileSet pj
                       (if (> (count p) 1)(nth p 1) {})
                       (if (> (count p) 2)(nth p 2) []))
           (.addFileset root))
      nil))
  root)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- maybeCfgNested
  ""
  [pj tk nested]

  (doseq [p nested]
    (case (first p)

      :compilerarg
      (when-some [n (:line (last p))]
        (-> (.createCompilerArg tk)
            (.setLine ^String n)))

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
      (->> (antFormatter pj (last p))
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

      :fileset
      (let [s (antFileSet
                pj
                (if (> (count p) 1)(nth p 1) {})
                (if (> (count p) 2)(nth p 2) []))]
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
      (->> (antJunitTest
             pj
             (if (> (count p) 1)(nth p 1) {})
             (if (> (count p) 2)(nth p 2) []))
           (.addTest tk))

      :chainedmapper
      (->> (antChainedMapper
             pj
             (if (> (count p) 1)(nth p 1) {})
             (if (> (count p) 2)(nth p 2) []))
           (.add tk))

      :targetfile
      (.createTargetfile tk)

      :srcfile
      (.createSrcfile tk)

      :batchtest
      (antBatchTest
        pj
        (.createBatchTest tk)
        (if (> (count p) 1)(nth p 1) {})
        (if (> (count p) 2)(nth p 2) []))

      :tarfileset
      (antTarFileSet
        pj
        (.createTarFileSet tk)
        (if (> (count p) 1)(nth p 1) {})
        (if (> (count p) 2)(nth p 2) []))

      nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- xxx-preopts "" [tk options] [options #{}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- delete-pre-opts
  ""
  [tk options]
  [(merge {:includeEmptyDirs true} options) #{}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- junit-preopts
  ""
  [^JUnitTask tk options]

  (if-some [v (:printsummary options)]
    (.setPrintsummary
      tk
      (doto
        (JUnitTask$SummaryAttribute.)
        (.setValue (str v)))))

  (if-some [v (:forkMode options)]
    (.setForkMode
      tk
      (doto
        (JUnitTask$ForkMode.)
        (.setValue (str v)))))

  [options #{:printsummary :forkMode}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- jdoc-preopts
  ""
  [^Javadoc tk options]
  (if-some [v (:access options)]
    (.setAccess
      tk
      (doto
        (Javadoc$AccessType.)
        (.setValue (str v)))))
  [options #{:access}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- tar-preopts
  ""
  [^Tar tk options]
  (if-some [v (:compression options)]
    (.setCompression
      tk
      (doto
        (Tar$TarCompressionMethod.)
        (.setValue (str v)))))
  [options #{:compression}])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- init-task
  "Reify and configure actual ant tasks"
  ^Task
  [^Project pj
   ^Target target
   {:keys [pre-options
           tname
           task
           options
           nested]}]
  (let [preopts (or pre-options
                    xxx-preopts)]
    (->> (doto ^Task
           task
           (.setProject pj)
           (.setOwningTarget target))
         (.addTask target))
    (->> (preopts task options)
         (apply setOptions pj task))
    (maybeCfgNested pj task nested)
    task))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- projAntTasks
  "Bind all the tasks to a target and a project"
  ^Target
  [^String target tasks]
  {:pre [(coll? tasks)]}
  (let [pj @dftprj
        tg (Target.)]
    (.setName tg (or target ""))
    (.addOrReplaceTarget ^Project pj tg)
    ;;(println (str "number of tasks ==== " (count tasks)))
    (doseq [t tasks]
      (init-task pj tg t))
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
  {:pre [(coll? tasks)]}
  (-> (projAntTasks target tasks)
      (execTarget)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn runTarget*
  "Run ant tasks" [target & tasks] (runTarget target tasks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn runTasks
  "Run ant tasks"
  [tasks] {:pre [(coll? tasks)]} (runTarget "" tasks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn runTasks* "Run ant tasks" [& tasks] (runTarget "" tasks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn- ctask<>
  ""
  ^Task
  [^Project p ^String tt ^String tm]
  (doto
    (.createTask p tt)
    (.setTaskName tm)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private ant-task
  "Generate wrapper function for an ant task"
  ([pj sym docstr func preopt]
   (let [s (str func)
         tm (cs/lower-case
              (.substring s
                          (inc (.lastIndexOf s "."))))]
     `(defn ~sym ~docstr
        {:no-doc true}
        [& [options# nested#]]
        (let [tk# (ctask<> ~pj ~s ~tm)
              o# (or options# {})
              n# (or nested# [])
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
            r#)))))
  ([pj sym docstr func]
   `(ant-task ~pj ~sym ~docstr ~func nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defmacro ^:private declAntTasks
  "Introspect the default project and cache all registered ant-tasks"
  [pj]
  `(do ~@(map (fn [[a b]]
                `(ant-task ~pj ~a "" ~b))
              (deref _tasks))))

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
       (runTasks* (antDelete
                    {:removeNotFollowedSymlinks true
                     :quiet quiet}
                    [[:fileset
                      {:followSymlinks false :dir dir}
                      [[:include "**/*"]]]]))
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
         (antDelete
           {:removeNotFollowedSymlinks true
            :quiet quiet}
           [[:fileset {:followSymlinks false :dir dir} ]]))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn copyFile
  "Copy a file to the target folder"
  [file toDir]
  (.mkdirs (io/file toDir))
  (runTasks*
    (antCopy {:file file
              :todir toDir})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn moveFile
  "Move a file to the target folder"
  [file toDir]
  (.mkdirs (io/file toDir))
  (runTasks*
    (antMove {:file file
              :todir toDir})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
(defn symLink
  "Create a file system symbolic link"
  [link target]
  (runTasks*
    (antSymlink {:overwrite true
                 :action "single"
                 :link link
                 :resource target})))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;EOF

