(ns cfg.core
  (:require [cfg.utils :as utils]
            [cfg.utils :refer [map-let fail-when fail-when-let]]
            [cfg.protocols :as proto]
            [cfg.types.core :refer [resolve-typemap process-mixins]]
            :reload-all))

(defn all-keys [conf]
  (loop [[[k v :as e] & more] (seq (:options conf))
         acc []]
    (if e
      (if (satisfies? proto/PConfig v)
        (recur more (into acc (map (partial into [k]) (all-keys v))))
        (recur more (conj acc [k])))
      acc)))

(defn get-defaults [conf]
  (into {}
    (filter identity
      (for [[k v] (:options conf)]
      (if (satisfies? proto/PConfig v)
        [k (get-defaults v)]
        (try [k ((:get-default k))] (catch Exception e nil)))))))

; (defn build-validation-queue [conf data]
;   (map-let conf [options]
;     ()))

(defrecord Config [options aliases basetype docstring]
  proto/GetWithin

  (get-within [{options :options} ks]
    (proto/get-within options ks))

  proto/PConfig

  (parse [conf data]
    (loop [[[k v :as e] & more] (seq data)
           acc {}]
      (if e
        (if-let [parser (:parse (options k))]
          (recur more (assoc acc k (parser v)))
          (recur more (assoc acc k (proto/parse (options k) v))))
        acc)))

  (validate [conf data]
    (map-let conf [options]
      (let [defaults (get-defaults conf)]))))

(defn add-opt [conf k more-aliases typevec]
  (map-let conf [aliases options basetype]
    (fail-when (options k)
      "Duplicate key " k)
    (fail-when-let [a (some (utils/keyset aliases) more-aliases)]
      "Duplicate cli alias " a)
    (let [typemap (resolve-typemap basetype (process-mixins typevec))]
      (-> conf
        (assoc-in [:options k] typemap)
        (assoc :aliases (into aliases (map #(do [% [k]]) more-aliases)))))))



(defn nest [conf k child-config]
  (map-let conf [options aliases]
    (fail-when (options k)
      "Duplicate key " k)
    (let [as (:aliases child-config)]
      (fail-when-let [a (some (utils/keyset aliases) as)]
        "Duplicate cli alias " a)
      (-> conf
        (assoc-in [:options k] child-config)
        (assoc :aliases (into aliases (for [[a ks] as]
                                        [a (into [k] ks)])))))))



(defn all-keys [conf]
  (loop [[[k v :as e] & more] (seq (:options conf))
         acc []]
    (if e
      (if (satisfies? proto/PConfig v)
        (recur more (into acc (map (partial into [k]) (all-keys v))))
        (recur more (conj acc [k])))
      acc)))

(defn build-validation-queue [conf data]
  (map-let conf [options]
    ()))



(defn parse-and-validate [conf data]
  (let [data (proto/parse conf data)]
    (proto/validate conf data)
    data))

(defn parse-cli-args [conf args]
  (let [aliases (:aliases conf)]
    (loop [[a & [b & more :as things]] args
           acc {}
           unused []]
      (if a
        (if (utils/is-cli-opt-flag? a)
          (if-let [ks (aliases a)]
            (let [typemap (proto/get-within conf ks)
                  parse   (:cli-parse typemap)]
              (utils/log-syms typemap parse ks aliases)
              (let [[value remaining] (parse things)]
                (recur remaining (assoc-in acc ks value) unused)))
            (utils/fail! a "is not an option"))
          (recur things acc (conj unused a)))
        [acc unused]))))

(defn- rewrite-opt
  "dissolves the syntactic sugar of the opt command within the config macro."
  [args]
  (let [[k & more] args
        [aliases more] (split-with symbol? more)
        [mixins docstring typeargs] (utils/get-mixins-and-docstring more)

        typedef (merge (apply hash-map typeargs)
                  (if docstring {:docstring docstring} {}))

        typevec (vec (flatten (conj mixins typedef)))
        aliases (mapv name aliases)]

    `(add-opt ~k ~aliases ~typevec)))

(defn- call? [sym form]
  (and
    (list? form)
    (= sym (first form))))

(defn- traverse
  "re-writes forms in the config macro"
  [body]
  (for [form body]
    (cond
      (call? 'opt form)
        (rewrite-opt (rest form))
      (call? 'opts form)
        `(nest ~(second form) (config ~@(drop 2 form)))
      :else (throw (IllegalArgumentException.
        "Config body can consist only of calls to `opt` and `opts`")))))

(defmacro config [& body]
  (let [[mixins docstring body] (utils/get-mixins-and-docstring body)]
    (utils/log-syms mixins docstring body)
    `(-> (Config. {} {} ~mixins ~docstring)
      ~@(traverse body))))

(defmacro defconfig [nm & body]
  `(def ~nm (config ~@body)))

(defmacro prgood [body]
  `(clojure.pprint/pprint (clojure.walk/macroexpand-all (quote ~body))))

; (prgood
;   (defconfig options
;     "some useful options"

;     (opt :tree-height -th --tree-height
;       "The height of the tree"
;       [float32 pos?])
    
;     (opts :general-things
;       "Some general things"
;       [int32]
;       (opt :species -s --species
;         [#{:monkey :father}]))))

; (defconfig myconfig
;   (opt :overwrite -r --overwrite [flag]
;     "Choose whether or not to overwrite the specified output file")
;   (opt :outpath -o --output-path
;     [int32 pos?]
;     "the output path"
;     :validate-with :overwrite
;     :validate-with-fn (fn [f overwrite?] (do nil))))


; (clojure.pprint/pprint 
;   (macroexpand '(opt :outpath -o --output-path
;     [thing etc]
;     "the output path"
;     :validate-with :overwrite
;     :validate-with-fn (fn [f overwrite?] (if (.exists (as-file f)))))))

