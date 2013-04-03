
(defn make-alias-map [as ks]
  (zipmap as (repeat ks)))

(defmacro rassoc-in [who field ks v]
  `(clojure.core/assoc-in ~who (clojure.core/into [~field] ~ks) ~v))

(defmacro rupdate-in [who field ks f & args]
  `(clojure.core/update-in ~who (clojure.core/into [~field] ~ks) ~f ~@args))

(defn someget [pred coll]
  (seq (filter pred coll)))

(defrecord cfg-context [docstring tasks args aliases opts metadata]

  (assoc-opt!
    "Associates an option in this context, without checking semantic validity."
    [me ks typedef]
    (-> me
      (assoc :aliases (merge aliases (make-alias-map ks (:aliases typedef))))
      (rassoc-in :opts ks (:default typedef))
      (rassoc-in :metadata ks typedef)))

  (assoc-opt
    "Associates an option in this context, checking for semantic validity first.
    Bad semantics cause IllegalArgumentException."
    [me ks typedef]
    (fail-when (get-in metadata ks) "There's already an option at " ks)
    (fail-when-let [bad (someget (complement legit-opt-prop?) (seq typedef))]
      "Illegal opt propert(y|ies): " ks)
    (fail-when-let [bad (someget (into #{} (:aliases typedef)) (keys aliases))]
      "Alias(es)? already in use: " bad)
    (assoc-opt! me ks typedef))

  (assoc-opts!
    "Associates an option group in this context, without validation."
    [me ks docstring]
    (-> me
      (rassoc-in :metadata ks {:__docstring docstring})
      (rassoc-in :opts ks {})))
  
  (assoc-opts
    "Associates an option group in this context, with validation."
    [me ks docstring]
    (fail-when (get-in metadata ks) "Option group at " ks " already exists")
    (assoc-opts! me ks docstring))
  
  (assoc-arg!
    "Associates an arg in this context, without validation."
    [me k typedef]
    (-> me
      (rassoc-in :opts [k] (:default typedef))
      (rupdate-in :args [] conj [k typedef])))
  
  (assoc-arg
    "Associates an arg in this context, with validation."
    [me k typedef]
    (fail-when (some (comp #{k} first) args)
      "Can't create arg. Key " k " already in use.")
    (assoc-arg! me k typedef))
  
  (assoc-task!
    "Associates a task in this context, without validation."
    [me nm ctx]
    (rassoc-in me :tasks [nm] ctx))
  
  (assoc-task
    "Associates a task in this context, with validation."
    [me nm ctx]
    (fail-when-not (symbol? nm) "Task names must be symbols.")
    (fail-when (tasks nm) "A task with name " nm " already exists.")
    (assoc-task! me nm ctx))
  
  (get-opts
    "Gets options. When given a seq of task symbols, get the options for the
    scope of the relevant task."
    ([me] (get-opts me []))
    ([me [t & others :as ts]]
      (if t
        (if-let [subcontext (tasks t)]
          (merge opts (get-opts subcontext others))
          (fail! "No task at " ts))
        opts)))
  
(defn ctxsym [] (gensym "__ctx"))
  
(defmacro config [& body]
  (let [docstring (when (string? (first body)) (first body))
        ctx-sym   (ctxsym)]
    `(clojure.core/let [~ctx-sym (clojure.core/atom (->cfg-context ~docstring {} [] {} {} {}))]
      ))
  )

(clojure.pprint/pprint (macroexpand '(config)))
(def def-form-expanders {
  'OPT (fn [ctxsym keypath []])
  'OPTS
  'ARG
  'TASK
  })

(config
  "A lovely configuration, Holmes."

  (TASK william
    (ARG :))

  (ARG :input-files multi in-file
    "A list of input files")

  (ARG :output-file out-file
    "The output file")

  (OPT :cheese int
    "The best kind of cheese is a homemade cheese"
    :aliases ["c" "-cheese"])

  (OPTS :banana-types
    "There are several types of banana"
    (OPT :cheesits multi
      "How many cheesits does it take to change a lightbulb?")))

(defn inner-def? [form]
  (and (list? form) (def-forms (first form))))

(defn expand-inner-defs [f form]
  (if (seq? form)
    (if (inner-def? form)
      (f form)
      (for [inner-form form] (expand-inner-defs f inner-form)))
    form))

(defmacro defconfig [id & body]
  (when-not (symbol? id) (fail! "The first argument to defconfig must be a symbol."))
  (let [docstring (when (string? (first body)) (first body))
        ctx-sym   (gensym "ctx")]
    `(clojure.core/let [~ctx-sym (clojure.core/atom (->cfg-context ~docstring {} {} {}))]
      (do ~@(expand-inner-defs
              (fn [[s k & others :as form]]
                `(clojure.core/reset! ~ctx-sym
                  ~(with-meta `(~s (deref ~ctx-sym) [~k] ~@others) (meta form))))
              body))
      (def ~id (deref ~ctx-sym)))))

(defn process-props [args]
  (let [[mixins [docstring & _ :as props]] (split-with symbol? args)
        docstring (when (string? docstring) docstring)
        properties (if docstring (cons :docstring props) props)
        base  (reduce merge {} (map @OPT_TYPES mixins))
        icing (into {} (map vec (partition 2 properties)))
        cake (merge base icing)]
    cake))

(defn quote-syms [coll]
  (mapv #(if (symbol? %) `(quote ~%) %) coll))

(defmacro defopt [ctx ks & body]
  (when (symbol? (last ks)) (fail! "Opt keys must not be symbols. Sorry."))
  (with-meta `(def-opt ~ctx ~(quote-syms ks) ~(process-props body)) (meta &form)))


(defmacro defopts [ctx ks & body]
  (when (symbol? (last ks)) (fail! "Opt keys must not be symbols. Sorry."))
  (let [[mixins [docstring & _ :as args]] (split-with symbol? body)
        docstring   (if (string? docstring) docstring nil)
        ctx-sym   (gensym "ctx")]
    `(clojure.core/let [~ctx-sym (clojure.core/atom (def-opts ~ctx ~(quote-syms ks) ~docstring))]
      (do ~@(expand-inner-defs
              (fn [[s k & others :as form]]
                (if (#{'defopt 'defopts} s)
                  `(reset! ~ctx-sym
                    ~(with-meta `(~s (deref ~ctx-sym) ~(conj ks k) ~@mixins ~@others) (meta form)))
                  (fail! (str "'" s "' not allowed inside defopts"))))
              args))
      (deref ~ctx-sym))))

(defmacro defarg [ctx ks & body]
  (when (symbol? (last ks)) (fail! "Arg keys must not be symbols. Sorry."))
  (with-meta `(def-arg ~ctx ~(quote-syms ks) ~(process-props body)) (meta &form)))


(defmacro deftask [ctx ks & body]
  (when-not (symbol? (last ks)) (fail! "Task names must be symbols"))
  (let [docstring (when (string? (first body)) (first body))
        ctx-sym   (gensym "ctx")]
    `(clojure.core/let [~ctx-sym (clojure.core/atom (def-task ~ctx ~(quote-syms ks) ~docstring))]
      (do ~@(expand-inner-defs
              (fn [[s k & others :as form]]
                `(clojure.core/reset! ~ctx-sym
                  ~(with-meta `(~s (deref ~ctx-sym) ~(conj ks k) ~@others) (meta form))))
              body))
      (deref ~ctx-sym))))

