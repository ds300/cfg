(ns cfg.types
  "Here be all the types I am supplying out of the box."
  (:require [cfg.types.core :refer [paramtype resolve-typemap]]
            [clojure.edn :as edn]
            :reload-all))

(defmacro defparamtype [nm & args]
  `(def ~nm (paramtype ~@args)))

(defn in-range [a b]
  (fn [x]
    (and (>= x a) (<= x b))))

(defn one-of [& ks]
  (paramtype [(into #{} ks)]
    :description (str "one of the following: "
                   (apply str (interpose ", " ks)))))

(defparamtype bool
  "Boolean flag"
  :take 0
  :validate (fn [b] (instance? Boolean b)))

(defparamtype integral
  "Integer"
  [integer?]
  :cli-parse edn/read-string)

(defparamtype int64 [integral]
  :parse long)

(defparamtype int32 [integral]
  :parse int)

(defparamtype decimal
  "Decimal value"
  [decimal?]
  :cli-parse edn/read-string)

(defparamtype float64 [decimal]
  :parse double)

(defparamtype float32 [decimal]
  "Floating Point Number"
  :parse float)

(defparamtype required
  :required true)

(defparamtype optional
  :requried false)

(defparamtype sequential
  :seq? true
  :seq-parse vec)

(defparamtype multi [sequential]
  :take -1)

(defparamtype tuple [sequential]
  :take 2
  :seq-validate #(= 2 (count %)))

(defparamtype triple [sequential]
  :take 3
  :seq-validate #(= 3 (count %)))

(defparamtype url
  "URL"
  :parse #(java.net.URL. %))

(defparamtype keyw [keyword?]
  "clojure keyword"
  :parse keyword
  :cli-parse keyword)

(defparamtype string
  "String"
  :add-validator string?)

(defparamtype csv [sequential]
  "comma-separated values"
  :cli-seq-parse #(clojure.string/split % #","))
