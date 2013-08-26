(ns cfg.types.core
  "This is the type system."
  (:require [cfg.utils :refer :all]
            [cfg.protocols :refer :all]
            [clojure.core.match :refer [match]]  :reload-all))


(def ^:dynamic *base-type* {})


(def recognised-keys
  #{
    :validators
    :validate-with
    :external-validate
    :parsers
    :take
    :cli-parse
    :seq-validate
    :cli-seq-parse
    :seq-parse
    :seq?
    :docstring
    :description
    :default
    :merge
    :required?
    :config-type
    :gen-default-fn
    :gen-default-with
    })

(defn merge-typemaps [a b]
  (loop [acc a [[k v :as e] & more] (seq b)]
    (if e
      (case k
        :add-validator
          (recur (update-in acc [:validators] (fnil conj []) v) more)
        :validate
          (recur (assoc acc :validators [v]) more)
        :add-parser
          (recur (update-in acc [:validators] (fnil conj []) v) more)
        :parse
          (recur (assoc acc :parsers [v]) more)
        (recur (conj acc e) more))
      acc)))


(defn make-args-parser
  "This function is kind of ok-ish now. Good job dave."
  [{n :take s-parse :cli-seq-parse parse :cli-parse :as typemap}]
  (case (or (and (integer? n) n) 1)
    0
      (complement (:get-default typemap))

    1
      (let [parser  (match [s-parse parse]
                      [nil nil] identity
                      [nil _]   parse
                      [_ nil]   s-parse
                      [_ _]     #(mapv parse (s-parse %)))]
        (fn [[a & args]]
          (fail-when-not a "no value given")
          [(parser a) args]))
    ; otherwise we are dealing with multi args
    (let [split-args (if (neg? n)
                      #(split-with (complement is-cli-opt-flag?) %)
                      #(split-at n %))
          parser     (or parse identity)]
      (fn [args]
        (let [[taken remaining] (split-args args)]
          [(parser taken) remaining])))))


(defn make-default-getter
  [{required :required default :default gen-default :gen-default-fn}]
  (cond
    required    #(fail! "No value supplied.")
    gen-default gen-default
    :else       (constantly default)))

(defn make-value-parser
  [{parsers :parsers s-parse :seq-parse}]
  (let [parse (case (count parsers)
                0 identity
                1 (first parsers)
                (apply comp parsers))]
    (if s-parse
      (fn [value]
        (mapv parse (s-parse value)))
      parse)))

(defn make-validator
  [{validators :validators ex-validate :external-validate sequence? :seq?}]
  (let [validate  (case (count validators)
                    0 (constantly true)
                    1 (first validators)
                    (apply pand validators))
        validate  (if sequence?
                    #(every? validate %)
                    validate)]
    (if ex-validate
      (fn [value & args]
        (and (validate value)
          (apply ex-validate value args)))
      validate)))

(defn resolve-typemap [typevec]
  (__> (reduce merge-typemaps *base-type* typevec)
    (assoc __ :get-default (make-default-getter __))
    (assoc __ :parse-args  (make-args-parser __))
    (assoc __ :parse       (make-value-parser __))
    (assoc __ :validator   (make-validator __))))

(defn process-mixins [ms]
  (vec
    (for [m (flatten ms)]
      (cond
        (map? m)
          m
        (satisfies? PConfig m)
          {:config-type m}
        (instance? clojure.lang.IFn m)
          {:add-validator m}
        :else (fail! "invalid mixin: " m)))))

(defn paramtype
  "makes a type vector"
  [& args]
  (let [[mixins description typeargs]  (get-mixins-and-docstring args)

        _                 (fail-when-not (even? (count typeargs))
                              "odd number of args")

        description       (if description
                            {:description description}
                            {})

        typedef           (merge description (apply hash-map typeargs))]

    (vec (flatten (conj (process-mixins mixins) typedef)))))