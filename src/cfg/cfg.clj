

(def ^:private OPT_TYPES (atom {}))

(defn legit-property? [[k v]]
  (case k
    :parse (fn? v)
    :validate (fn? v)
    :validate-with (and (vector? v) (every? vector? v))
    :aliases (and (vector? v) (every? string? v))
    :take (integer? v)
    :take-while (fn? v)
    :merge (fn? v)
    :description (string? v)
    :private? true
    :-optional true
    :default true
    :seq-parse (fn? v)
    false))

(defn ensure-legit-property [p]
  (if (legit-property? p)
    p
    (throw (IllegalArgumentException. "Illegal property definition."))))

(def base-type {:take 1, :default nil})

(defmacro defopttype [tsym & args]
  (let [[mixins properties] (split-with symbol? args)]
    ; make sure we can shove the properties in a map
    (when-not (even? (count properties))
      (throw (IllegalArgumentException. "defopttype expects an even number of forms")))
    ; makse sure the mixins are actual opttypes.
    (when-let [bad (seq (filter (complement @OPT_TYPES) mixins))]
      (throw (IllegalArgumentException. (str "Undefined opttype(s): " bad))))
    
    (let [properties (into {}
                       (map (comp ensure-legit-property eval vec)
                         (partition 2 properties)))]
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
  :-optional true)

(defopttype csv
  :seq-parse #(clojure.string/split (first %) #","))

(defopttype multi
  :seq-parse vec)

(defprotocol CFGContext
  (merge-public [me m] [me ks m] "Merges a map, validating the blah etc.")
  (get-opt [me & ks] "gets an opt")
  (set-opt [me ks v] "sets an opt, validating")
  (set-opt! [me ks v] "sets an opt without validating")
  (get-task-context [me task]))

(defrecord cfg-context [docstring aliases metadata opts args])

(def def-forms {'defopt 'defopt- 'defarg 'defarg- 'deftask 'deftask- 'defopts 'defopts-})

(defmacro defconfig [id & args]
  `(clojure.core/let [ctx# (clojure.core/atom
                             (->cfg-context
                               ~(when (string? (first args)) (first args))
                               {} {} {} {}))]
     (do
      ~@(for [a args]
          (if (and (list? a) (def-forms (first a)))
            (let [[s k & others] a]
              (cons (def-forms s) `(ctx# [~k] ~@others)))
            a)))
     (def ~id @ctx#)))

