(ns cfg.types
  "Yo. This is the type system of cfg."
  (:require [cfg.utils :refer :all]))

(def base-type {})

(defmacro error-if-not [thing msg]
  `(when-not ~thing
    (-> (str "invalid parameter type definition: " ~msg)
      IllegalArgumentException.
      throw)))



(defn merge-typedefs [a b]
  (loop [acc a [[k v :as e] & more] (seq b)]
    (if e
      (case k
        :add-validator
          (recur (update-in acc [:validators] (fnil conj []) v) more)
        ; default
        (recur (conj acc e) more))
      acc)))

(defmacro validate-sugar [sugar]
  `(do
    (error-if-not (every? #{java.lang.String clojure.lang.PersistentVector}
                     (keys ~sugar))
      "expecting either description, mixin list, both, or none")

    (error-if-not (every? #(= 1 (count %)) (vals ~sugar))
      "too many arguments given")))

(defn process-mixins [ms]
  (vec
    (for [m ms]
      (if ((por fn? set?) m)
        {:add-validator m}
        m))))

(defn make-typedef
  "makes a type definition"
  [& args]
  (let [[sugar typeargs]  (split-with (complement keyword?) args)

        _                 (error-if-not (even? (count typeargs))
                              "odd number of args")

        typedef           (apply hash-map typeargs)

        sugar             (group-by type sugar)

        _                 (validate-sugar sugar)

        mixins            (process-mixins
                            (or (first (sugar clojure.lang.PersistentVector)) []))

        description       (if-let [d (first (sugar java.lang.String))]
                            {:description d}
                            {})

        base              (reduce merge-typedefs base-type (conj mixins description))]

    (merge-typedefs base typedef)))

(defmacro defparamtype [nm & args]
  `(def ~nm (make-typedef ~@args)))

;; do diferent types for cmd and json



(defparamtype bool
  "Boolean"
  :default false
  :parse #(Boolean. %))

; (defparamtype int64
;   "Integer"
;   :parse #(Long. %))

; (defparamtype int32
;   [integer]
;   :parse #(Integer. %))

; (defparamtype uint
;   "Integer >= 0"
;   [integer]
;   :validators [#(>= % 0)])

; (defparamtype uint32
;   [uint]
;   :parse #(Integer. %))

; (defparamtype nint
;   "Integer >= 1"
;   [integer]
;   :validators [pos?])

; (defparamtype nint32
;   [nint]
;   :parse #(Integer. %))

; (defparamtype csv)


; (defn ok [& args] true)

; (defn validators [& args]
;   (into {}
;     (for [[k validate description] (partition-all 3 args)]
;       [k [validate description]])))

; (def base-validators
;   (validators
;     :parse       (por nil? fn?)                        "function or nil"
;     :seq-parse   (por nil? fn?)                        "function or nil"
;     :validate    (por nil? fn?)                        "function or nil"
;     :aliases    #(and (vector? %) (every? string? %))  "vector of strings"
;     :take        integer?                              "integer"
;     :take-while  fn?                                   "function"
;     :merge       fn?                                   "function"
;     :description string?                               "string"
;     :docstring   string?                               "string"
;     :default     ok                                    "anything"
; ))


; (defn legit-pred-maker [validmap]
;   (fn [[k v]]
;     (let [[validate _] (validmap k)]
;       (and validate (validate v)))))

; (defn prop-ensurer-maker [validmap pred]
;   (fn [[k v :as thing]]
;     (if (pred thing)
;       thing
;       (fail!
;         (let [[_ description] (validmap k)]
;           (if description
;             (str "Bad argument property value for " k ". Expecting " description ".")
;             (str "Trying to set nonexistent argument property " k)))))))

; (def ensure-legit-arg-prop (prop-ensurer-maker arg-prop-validators legit-arg-prop?))
; (def ensure-legit-opt-prop (prop-ensurer-maker opt-prop-validators legit-opt-prop?))


; (def base-type {:take 1, :default nil})

; (defmacro defthingtype [thing tsym & args]
;   (let [[mixins properties] (split-with symbol? args)]
;     ; make sure we can shove the properties in a map
;     (fail-when-not (even? (count properties)) "defopttype expects an even number of forms")
;     ; makse sure the mixins are actual opttypes.
;     (fail-when-let [bad (seq (filter (complement @OPT_TYPES) mixins))]
;       "Undefined opttype(s): " bad)
    
;     (let [ensure-legit-prop (eval (symbol (str "ensure-legit-" thing "-prop")))
;           properties  (into {}
;                         (map (comp ensure-legit-prop eval vec)
;                           (partition 2 properties)))]
;       (swap! OPT_TYPES assoc tsym
;         (merge
;           (reduce merge {} (map @OPT_TYPES mixins))
;           properties))))
;   true)

; (defn validate-type-map [typedef]
;   (cond
;     ()
;     :else :everythings-a-okay))



; (defn create-type-map [mixins typedef]
;   (let [typedef (merge (reduce merge {} mixins) typedef)]
;     (do (validate-type-map typedef)
;       typedef)))

; (defmacro defopttype [nm & args]
;   `(def ~nm (create-type-map 
;               ~(take-while symbol? args)
;               ~(into {} (drop-while symbol? args)))))


; (defopttype int32
;   :parse #(Integer. %)
;   :description "integer")

; (defopttype uint32
;   int
;   :validate #(>= % 0)
;   :description "integer >= 0")

; (defopttype nint32 
;   int
;   :validate pos?
;   :description "integer > 0")

; (defopttype int
;   :parse #(Long. %)
;   :description "integer")

; (defopttype uint
;   int
;   :validate #(>= % 0)
;   :description "integer >= 0")

; (defopttype nint 
;   int
;   :validate pos?
;   :description "integer > 0")


; (defopttype flag
;   :take 0
;   :merge (fn [a b] (not a)))

; (defopttype optional
;   :optional? true)

; (defopttype csv
;   :seq-parse #(clojure.string/split (first %) #","))

; (defopttype colon-sv
;   :seq-parse #(clojure.string/split (first %) #":"))

; (defopttype psv
;   :seq-parse #(clojure.string/split (first %) #"\."))

; (defopttype multi
;   :seq-parse vec
;   :take nil)

; (defn unbounded-multi-arg? [typedef]
;   (and
;     (:seq-parse typedef)
;     (not (:take typedef))))
