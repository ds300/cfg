(ns cfg.types
  "Yo. This is the type system of cfg."
  (:require [cfg.utils :refer :all]
            [cfg.protocols :refer :all]))

(def ^:dynamic *base-type* {})

(extend-type clojure.lang.IPersistentMap
  Type
  (parse-cli-args [me [head & tail :as args]]
    (let [n (:take me 1)
          from-string (:from-string me identity)
          seq-from-string (:seq-from-string me)]
      (if seq-from-string
        ))))

(def recognised-keys
  #{
    :validators
    :take
    :validate-with
    :validate-with-fn
    :seq-validate
    :seq-from-string
    :seq-parse
    :add-parser
    :seq?
    :parsers
    :add-validator
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

(defn resolve-typemap [typevec]
  (->Type (reduce merge-typemaps *base-type* typevec)))

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

(defn make-typevec
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
  `(def ~nm (make-typevec ~@args)))


(defparamtype bool
  "Boolean flag"
  :default false
  :take 0
  :merge (fn [a b] (not a))
  :from-string #(Boolean. %)
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

(defparamtype keyw
  "clojure keyword"
  :add-validator keyword?
  :from-string keyword)

(defparamtype csv [sequential]
  "comma-separated values"
  :from-string #(clojure.string/split % #","))