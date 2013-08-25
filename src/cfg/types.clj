(ns cfg.types
  "Yo. This is the type system of cfg."
  (:require [cfg.utils :refer :all]
            [cfg.protocols :refer :all]
            [clojure.edn :as edn]
            [clojure.core.match :refer [match]]))

(def ^:dynamic *base-type* {})

(def recognised-keys
  #{
    :validators
    :validate-with
    :validate-with-fn
    :add-validator
    :parsers
    :add-parser
    :parse-with
    :parse-with-fn
    :take
    :seq-validate
    :seq-from-string
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
        ; add-validator adds a validator
        :add-validator
          (recur (update-in acc [:validators] (fnil conj []) v) more)
        ; validator replaces the preceding vector of validators
        :validator
          (recur (assoc acc :validators [v]) more)
        ; default
        (recur (conj acc e) more))
      acc)))

(defn get-multi-args [args]
  (fn [args]
    (split-with (complement #(.startsWith % "-")) args)))

(defn with-one [f]
  (fn [[a & args]]
    (fail-when-not a "no value given")
    [(f a) args]))

(defn with-some [parse-fn]
  (fn [args]
    (let [[taken remaining] (get-multi-args args)]
      [(parse-fn taken) remaining])))

(defn with-n [n parse-fn]
  (fn [args]
    (let [[taken remaining] (split-at n args)]
      [(parse-fn taken) remaining])))

(defn make-args-parser
  "This function is an horrific beast. I am so sorry future me."
  [{n :take s-parse :seq-from-string parse :from-string :as typemap}]
  (let [f (case (or n 1)
            0
              (let [default   (:default typemap)
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
              (with-fn parser)))]
    (-> typemap
      (assoc :from-args-fn f)
      (dissoc :from-string :seq-from-string :take))))

(defn resolve-typemap [typevec]
  (-> (reduce merge-typemaps *base-type* typevec)
    make-from-args-fn))

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
        :else (fail! "invalid mixin: " m))))))

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

(defmacro defparamtype [nm & args]
  `(def ~nm (paramtype ~@args)))

(defn in-range? [a b]
  (fn [x]
    (and (>= x a) (<= x b))))

(defn one-of [& ks]
  (paramtype [(into #{} ks)]
    :description (str "one of the following: " (apply str (interpose ", " ks)))))

(defparamtype bool
  "Boolean flag"
  :take 0
  :from-string (fn [] true)
  :validate (fn [b] (instance? Boolean b)))

(defparamtype integral
  "Integer"
  [integer?]
  :from-string edn/read-string)

(defparamtype int64 [integral]
  :parse long)

(defparamtype int32 [integral]
  :parse int)

(defparamtype decimal
  "Decimal value"
  [decimal?]
  :from-string edn/read-string)

(defparamtype float64 [decimal]
  :parse double)

(defparamtype float32 [decimal]
  "Floating Point Number"
  :parse float)

(defparamtype required
  :required true)

(defparamtype sequential
  :seq true
  :seq-parse vec)

(defparamtype multi [sequential]
  :take -1)

(defparamtype tuple [sequential]
  :take 2)

(defparamtype triple [sequential]
  :take 3)

(defparamtype url
  "URL"
  :from-string #(java.net.URL. %))

(defparamtype keyw [keyword?]
  "clojure keyword"
  :from-string keyword)

(defparamtype string
  "String"
  :add-validator string?)

(defparamtype csv [sequential]
  "comma-separated values"
  :from-string #(clojure.string/split % #","))