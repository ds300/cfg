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
    :validate-with-fn
    :parsers
    :parse-with
    :parse-with-fn
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

(defn with-one [f]
  (fn [[a & args]]
    (fail-when-not a "no value given")
    [(f a) args]))

(defn with-some [parse-fn]
  (fn [args]
    (let [[taken remaining] (split-with (complement #(.startsWith % "-")) args)]
      [(parse-fn taken) remaining])))

(defn with-n [n parse-fn]
  (fn [args]
    (let [[taken remaining] (split-at n args)]
      [(parse-fn taken) remaining])))

(defn make-args-parser
  "This function is an horrific beast. I am so sorry future me."
  [{n :take s-parse :cli-seq-parse parse :cli-parse :as typemap}]
  (case (or n 1)
    0
      (let [get-default   (:get-default typemap)
            val-expr  (match [parse default]
                        [nil nil] nil
                        [nil _]   default
                        [_ _]     `(~parse))]
        (eval `(fn [args#] [~val-expr args#])))

    1
      (let [parser  (match [s-parse parse]
                      [nil nil] identity
                      [nil _]   parse
                      [_ nil]   s-parse
                      [_ _]     #(mapv parse (s-parse %)))]
        (with-one parser))
    ; otherwise
    (let [with-fn (if (neg? n) with-some (partial with-n n))
          parser  (match [s-parse parse]
                    [nil nil] identity
                    [nil _]   (partial mapv parse)
                    [_ nil]   (partial mapv s-parse)
                    [_ _]     #(mapv (partial mapv parse) (map s-parse %)))]
      (with-fn parser))))

(defn required-val [parser])

(defn make-default-getter
  [{required :required default :default gen-default :gen-default-fn}]
  (cond
    required    (fn [] (-> "No value supplied."
                          IllegalArgumentException.
                          throw))
    gen-default gen-default
    :else       (constantly default)))

(defn make-value-parser
  [{parsers :parsers parse-with-fn :parse-with-fn s-parse :seq-parse}]
  (let [parse  (case (count parsers)
                 0 identity
                 1 (first parsers)
                 (apply comp parsers))
        parse (if s-parse
                (fn [value]
                  (mapv parse (s-parse value)))
                parse)]
    (if parse-with-fn
      (fn [value & withs]
        (apply parse-with-fn (parse value) withs))
      parse)))

(defn make-validator
  [{validators :validators validate-with-fn :validate-with-fn s-val :seq-validate}]
  (let [validate  (case (count validators)
                 0 identity
                 1 (first parsers)
                 (apply comp parsers))
        parse (if s-parse
                (fn [value]
                  (mapv parse (s-parse value)))
                parse)]
    (if parse-with-fn
      (fn [value & withs]
        (apply parse-with-fn (parse value) withs))
      parse)))

(defn resolve-typemap [typevec]
  (__> (reduce merge-typemaps *base-type* typevec)
    (assoc __ :get-default (make-default-getter __))
    (assoc __ :parse-args (make-args-parser __))
    (assoc __ :parse (make-value-parser __))))

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