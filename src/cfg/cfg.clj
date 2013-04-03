

(def ^:private OPT_TYPES (atom {}))

(defmacro opttype [s]
  (let [r (@OPT_TYPES s)]
    `(do ~r)))

(defn fail! [& args]
  (throw (IllegalArgumentException. (apply str args))))

(defmacro fail-when [test & args]
  `(clojure.core/when ~test
    (fail! ~@args)))

(defmacro fail-when-let [bind & args]
  `(clojure.core/when-let ~bind
    (fail! ~@args)))

(defmacro fail-when-not [test & args]
  `(fail-when (clojure.core/not ~test) ~@args))

(def base-prop-validators
  {
    :parse fn?
    :seq-parse fn?
    :validate fn?
    :alisases #(and (vector? %) (every? string? %))
    :take integer?
    :take-while fn?
    :merge fn?
    :description string?
    :docstring string?
    :default #(do true)
   })

(def opt-prop-validators (merge base-prop-validators
  {
    :private? #(do true)
  }
  ))

(def arg-prop-validators (merge base-prop-validators
  {
    :optional? #(do true)
  }
  ))

(defn legit-opt-prop? [[k v]]
  (if-let [check (opt-prop-validators k)]
    (check v)
    false))

(defn legit-arg-prop? [[k v]]
  (if-let [check (arg-prop-validators k)]
    (check v)
    false))

; (defn parse-option [typedef current-value args]
;   (let [$         typedef
;         taken     (if-let [take-pred (:take-while $)]
;                     (take-while take-pred args)
;                     (take (:take $) args))
;         n         (count taken)
;         remaining (drop n args)
;         parse     (or (:parse $) identity)
;         merge     (or (:merge $) (fn [a b] b))
;         seq-parse (:seq-parse $)
;         things    (if seq-parse
;                     (seq-parse taken)
;                     (if (= 1 n)
;                       (first taken)
;                       taken))
;         parsed    (if seq-parse
;                     (mapv parse things)
;                     (parse things))
;         merged    (merge current-value parsed)]
;     [merged remaining]))

; (defn parse-argument [typedef current-value args]
;   (let [$         typedef
;         taken     (if-let [take-pred (:take-while $)]
;                     (take-while take-pred args)
;                     (take (:take $) args))
;         n         (count taken)
;         remaining (drop n args)
;         parse     (or (:parse $) identity)
;         merge     (or (:merge $) (fn [a b] b))
;         seq-parse (:seq-parse $)
;         things    (if seq-parse
;                     (seq-parse taken)
;                     (if (= 1 n)
;                       (first taken)
;                       taken))
;         parsed    (if (:optional? $)
;                     (try
;                       (if seq-parse
;                         (mapv parse things)
;                         (parse things))
;                       (catch Exception e nil)))]
;     (if (nil? parsed)
;       [nil args]
;       [(merge current-value parsed) remaining])))


(defn ensure-legit-property [p]
  (if (legit-opt-property? p)
    p
    (fail! "Illegal property definition.")))

(defn process-opt-properties [properties]
  (into {}
    (map (comp ensure-legit-property eval vec)
      (partition 2 properties))))

(def base-type {:take 1, :default nil})

(defmacro defopttype [tsym & args]
  (let [[mixins properties] (split-with symbol? args)]
    ; make sure we can shove the properties in a map
    (fail-when-not (even? (count properties)) "defopttype expects an even number of forms")
    ; makse sure the mixins are actual opttypes.
    (when-let [bad (seq (filter (complement @OPT_TYPES) mixins))]
      (fail! "Undefined opttype(s): " bad))
    
    (let [properties (process-opt-properties properties)]
      (swap! OPT_TYPES assoc tsym
        (merge
          (reduce merge {} (map @OPT_TYPES mixins))
          properties))))
  true)

(defopttype int
  :parse #(Integer. %)
  :description "integer")

(defopttype uint
  int
  :validate #(>= % 0)
  :description "integer >= 0")

(defopttype natint 
  int
  :validate #(> % 0)
  :description "integer > 0")

(defopttype flag
  :take 0
  :merge (fn [a b] (not a)))

(defopttype optional
  :optional? true)

(defopttype csv
  :seq-parse #(clojure.string/split (first %) #","))

(defopttype multi
  :seq-parse vec)



(defprotocol CFGContext
  (merge-public [me m] [me ks m] "Merges a map, validating the blah etc.")
  (get-opt [me & ks] "gets an opt")
  (def-opt [me ks typedef])
  (def-opt! [me ks typedef])
  (def-arg [me ks typedef])
  (def-arg! [me ks typedef])
  (def-opts [me ks docstring])
  (def-opts! [me ks docstring])
  (def-task [me ks docstring])
  (def-task! [me ks docstring])
  (parse-cli-args [me args] [me args dispatches])
  (scopes [me] [me ks])
  (key-in-scope? [me ks])
  (set-opt [me ks v] "sets an opt, validating")
  (set-opt! [me ks v] "sets an opt without validating")
  (get-task-context [me task]))

(defn parent-paths [[k & ks]]
  (when k
    (cons [k]
      (map #(into [k] %)
        (lazy-seq (parent-paths ks))))))

(defn symbol-scopes [m]
  (when (map? m)
    (when-let [kids (seq (filter (comp symbol? first) (seq m)))]
      (apply concat
        (for [[k v] kids]
          (map #(into [k] %)
            (cons [] (symbol-scopes v))))))))

(defn make-alias-map [ks as]
  (if (seq as)
    (into {} (for [a as] [a ks]))
    {}))

(defmacro rassoc-in [who field ks v]
  `(clojure.core/assoc-in ~who (clojure.core/into [~field] ~ks) ~v))

(defmacro rupdate-in [who field ks f & args]
  `(clojure.core/update-in ~who (clojure.core/into [~field] ~ks) ~f ~@args))

(defn someget [pred coll]
  (seq (filter pred coll)))

(defrecord context [docstring tasks args aliases opts metadata]

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
      (rupdate-in :args [] conj k)
      (rassoc-in :metadata [k] typedef)))
  
  (assoc-arg
    "Associates an arg in this context, with validation."
    [me k typedef]
    (fail-when (metadata k) "Can't create arg. Key " k " already in use.")
    )
  
  (assoc-task! [me nm ctx]
    (rassoc-in me :tasks [nm] ctx))
  (get-opts
    ([me] (get-opts me []))
    ([me [t & others :as ts]]
      (if t
        (if-let [subcontext (tasks t)]
          (merge opts (get-opts subcontext others))
          (fail! "No task at " ts))
        opts)))

(defrecord cfg-context [docstring aliases metadata opts]
  CFGContext

  (scopes [me] (scopes me []))

  (scopes [me ks]
    (let [this-scope (filterv symbol? ks)]
      (concat
          (cons [] (parent-paths this-scope))
          (symbol-scopes (get-in metadata this-scope)))))

  (key-in-scope? [me ks]
    (let [top (->> ks
                reverse
                (take-while (complement symbol?))
                reverse)]
      (boolean
        (some identity
          (for [s (scopes me ks)]
            (get-in metadata (into s top)))))))
  
  (def-opt! [me ks typedef]
    (-> me
      (assoc :aliases  (reduce #(assoc-in %1 %2 ks) aliases
                          (map
                            #(conj (filterv symbol? ks) %)
                            (:aliases typedef))))
      (assoc :metadata (assoc-in metadata ks typedef))
      (assoc :opts     (assoc-in opts ks (:default typedef)))))

  (def-opt [me ks typedef]
    (let [as (:aliases typedef)]
      (dorun (map ensure-legit-property (seq typedef)))
      (when (seq as)
        (dorun (for [a as s (scopes me ks)]
                 (fail-when (get-in aliases (conj s a))
                    "Alias, \"" a "\" already exists in this scope."))))
      (fail-when (key-in-scope? me ks) "Option at " ks " already exists in this scope.")
      (fail-when (:optional typedef) "Option at " ks " has :optional true. This is reserved for args.")
      (def-opt! me ks typedef)))

  (def-opts! [me ks docstring]
    (assoc me (assoc-in metadata (conj ks :__docstring) docstring)))

  (def-opts [me ks docstring]
    (fail-when (key-in-scope? me ks) "There is already an option at " ks)
    (def-opts! me ks docstring))

  (def-arg! [me ks typedef]
    (let [p (conj (vec (butlast ks)) :__args)
          k (last ks)]
      (-> me
        (assoc-in (into [:metadata] ks) :arg)
        (update-in (into [:metadata] p) (fnil conj []) [k typedef]))))

  (def-arg [me ks typedef]
    (fail-when-not (or (= 1 (count ks)) (symbol? (last (butlast ks))))
      "inline args must be declared at the top level of a config or a task")
    ; TODO more verification. i.e. semantics etc
    (fail-when (key-in-scope? me ks) "Cannot create arg. Key " ks " already in scope.")
    (def-arg! me ks typedef))

  (def-task! [me ks docstring]
    (assoc me :metadata (assoc-in metadata (conj ks :__docstring) docstring)))

  (def-task [me ks docstring]
    (fail-when-not (symbol? (last ks)) "Task names must be a symbol.")
    ; TODO more verification
    (def-task! me ks docstring))


  (parse-cli-args [me args]
    (let [ctx (atom me)
          taskpath (atom [])
          current-args (atom (metadata :__args))
          current-tasks (atom (into #{} (filter symbol? (keys metadata))))
          reset-ct!     #(reset! current-tasks (into #{} (filter symbol? (keys (get-in metadata @taskpath)))))]
      (loop [[arg & more] args]
        (when arg
          (if (and (not-empty current-tasks) (empty? current-args))
            (let [task (symbol arg)]
              (if-not (@current-tasks task)
                (fail! "\""arg"\" is not a valid task")
                (do
                  (swap! taskpath conj task)
                  (reset-ct!)
                  (reset! current-args (get-in metadata @taskpath))
                  (recur more))))
            (if (empty? current-args)))))))
  )


(def def-forms #{
  'defopt
  'defopts
  'defarg
  'deftask
  })

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

