(ns cfg.core
  (:require [cfg.utils :refer :all]
            [cfg.protocols :refer :all]
            [cfg.types :refer :all]
            :reload-all))



(defrecord Config [options aliases basetype docstring]
  GetWithin

  (get-within [me ks]
    (get-within options ks)))

  PConfig

  (add-opt [me k as typevec]
    (fail-when (options k)
      "Duplicate key " k)
    (fail-when-let [a (some (keyset aliases) as)]
          "Duplicate cli alias " a)
    (binding [*base-type* basetype]
      (let [typemap (resolve-typemap typevec)]
        (-> me
          (assoc-in [:options k] typemap)
          (assoc :aliases (into aliases (map #(do [% [k]]) as)))))))

  (nest [me k config]
    (fail-when (options k)
      "Duplicate key " k)
    (let [as (:aliases config)]
      (fail-when-let [a (some (keyset aliases) as)]
        "Duplicate cli alias " a)
      (-> me
        (assoc-in [:options k] config)
        (assoc :aliases (into aliases (for [[a ks] as]
                                        [a (into [k] ks)]))))))

  (merge-configs [me other]
    (Config.
      (apply merge (map :options [me other]))
      (apply merge (map :aliases [me other]))
      (:basetype other)
      (:docstring other)))


  (parse-cli-args [me args]
    (loop [[a & [b & more :as things]] args
           errors []
           acc {}
           unused []]
      (if a
        (if-let [ks (aliases a)]
          (let [typemap (get-within me ks)
                default (:default typemap)
                n       (or (:take typemap) 1)
                parser  (or (:from-string typemap) identity)])
          (recur things errors acc (conj unused a)))
        acc)))

(defn- rewrite-opt
  "dissolves the syntactic sugar of the opt command within the config macro."
  [args]
  (let [[k & more] args
        [aliases more] (split-with symbol? more)
        [mixins docstring typeargs] (get-mixins-and-docstring more)

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
  (let [[mixins docstring body] (get-mixins-and-docstring body)]
    `(-> (Config. {} {} (reduce merge-typedefs {} ~mixins) ~docstring)
      ~@(traverse body))))

(defmacro defconfig [nm & body]
  `(def ~nm (config ~@body)))

(defmacro prgood [body]
  `(clojure.pprint/pprint (clojure.walk/macroexpand-all (quote ~body))))

(prgood
  (defconfig options
    "some useful options"

    (opt :tree-height -th --tree-height
      "The height of the tree"
      [float32 pos?])
    
    (opts :general-things
      "Some general things"
      [int32]
      (opt :species -s --species
        [#{:monkey :father}]))))


(defconfig myconfig
  (opt :overwrite -r --overwrite [flag]
    "Choose whether or not to overwrite the specified output file"
    )
  (opt :outpath -o --output-path
    [int32 pos?]
    "the output path"
    :validate-with :overwrite
    :validate-with-fn (fn [f overwrite?] (do nil))))


(clojure.pprint/pprint 
  (macroexpand '(opt :outpath -o --output-path
    [thing etc]
    "the output path"
    :validate-with :overwrite
    :validate-with-fn (fn [f overwrite?] (if (.exists (as-file f)))))))

