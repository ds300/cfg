(ns cfg.types
  "Yo. This is the type system of cfg."
  (:require [cfg.utils :refer :all]))

(def TYPES (atom {}))

(defn ok [& args] true)

(defn validators [& args]
  (into {}
    (for [[k validate description] (partition-all 3 args)]
      [k [validate description]])))

(def base-validators
  (validators
    :parse       fn?                                   "function"
    :seq-parse   fn?                                   "function"
    :validate    fn?                                   "function"
    :alisases    #(and (vector? %) (every? string? %)) "vector of strings"
    :take        integer?                              "integer"
    :take-while  fn?                                   "function"
    :merge       fn?                                   "function"
    :description string?                               "string"
    :docstring   string?                               "string"
    :default     ok                                    "anything"
))

(def opt-prop-validators (merge base-validators (validators
  :private?  ok                          "anything"
  :starts-with (por re-pattern? string?) "regex pattern or string"
)))

(def arg-prop-validators (merge base-validators (validators
  :optional? ok                   "anything"
  :take      (pand integer? pos?) "integer > 0"
)))

(defn legit-pred-maker [validmap]
  (fn [[k v]]
    (let [[validate _] (validmap k)]
      (and validate (validate v)))))

(def legit-arg-prop? (legit-pred-maker arg-prop-validators))

(def legit-opt-prop? (legit-pred-maker opt-prop-validators))

(defn prop-ensurer-maker [validmap pred]
  (fn [[k v :as thing]]
    (if (pred thing)
      thing
      (fail!
        (let [[_ description] (validmap k)]
          (if description
            (str "Bad argument property value for " k ". Expecting " description ".")
            (str "Trying to set nonexistent argument property " k)))))))

(def ensure-legit-arg-prop (prop-ensurer-maker arg-prop-validators legit-arg-prop?))
(def ensure-legit-opt-prop (prop-ensurer-maker opt-prop-validators legit-opt-prop?))


(def base-type {:take 1, :default nil})

(defmacro defthingtype [thing tsym & args]
  (let [[mixins properties] (split-with symbol? args)]
    ; make sure we can shove the properties in a map
    (fail-when-not (even? (count properties)) "defopttype expects an even number of forms")
    ; makse sure the mixins are actual opttypes.
    (fail-when-let [bad (seq (filter (complement @OPT_TYPES) mixins))]
      "Undefined opttype(s): " bad)
    
    (let [ensure-legit-prop (eval (symbol (str "ensure-legit-" thing "-prop")))
          properties  (into {}
                        (map (comp ensure-legit-prop eval vec)
                          (partition 2 properties)))]
      (swap! OPT_TYPES assoc tsym
        (merge
          (reduce merge {} (map @OPT_TYPES mixins))
          properties))))
  true)

(defmacro defopttype [& args]
  `(defthingtype "opt" ~@args))

(defmacro defargtype [& args]
  `(defthingtype "arg" ~@args))

(defopttype int
  :parse #(Integer. %)
  :description "integer")

(defopttype uint
  int
  :validate #(>= % 0)
  :description "integer >= 0")

(defopttype natint 
  int
  :validate pos?
  :description "integer > 0")

(defopttype flag
  :take 0
  :merge (fn [a b] (not a)))

(defopttype optional
  :optional? true)

(defopttype csv
  :seq-parse #(clojure.string/split (first %) #","))

(defopttype colon-sv
  :seq-parse #(clojure.string/split (first %) #":"))

(defopttype psv
  :seq-parse #(clojure.string/split (first %) #"\."))

(defopttype multi
  :seq-parse vec)

