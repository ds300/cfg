(ns cfg.core
  (:require [cfg.utils :refer :all]
            [cfg.types :refer [merge-typedefs process-mixins]]
            [cfg.types.cli :refer :all]))

(defprotocol PConfig
  (add-opt [me k optdef])
  (nest [me k config]))

(defrecord Config [options aliases basetype docstring]
  PConfig
  (add-opt [me k optdef]
    (fail-when (options k) (str "Duplicate key " k))
    (let [as (:aliases optdef)]
      (fail-when-let [a (some aliases as)] (str "Duplicate cli alias " a))
      (-> me
        (assoc-in [:options k] optdef)
        (assoc :aliases (into aliases (map #([% k]) as))))))
  (nest [me k config]))

(defn- rewrite-opt
  "resolves the syntactic sugar of the opt command within the config macro.
  sym is the symbol bound to the Config which the opt is being defined for"
  [args]
  (let [[k & more] args
        [aliases more] (split-with symbol? more)
        [mixins docstring typeargs] (get-mixins-and-docstring more)

        typedef (reduce merge (apply hash-map typeargs)
                  [{:aliases (mapv name aliases)}
                  (if docstring {:docstring docstring} {})])]

    `(add-opt ~k (reduce merge-typedefs ~typedef (process-mixins ~mixins)))))

(defn- call? [sym form]
  (and
    (list? form)
    (= sym (first form))))

(defn- traverse [body]
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