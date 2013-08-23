(ns cfg.core
  (:require [cfg.utils :refer :all]
            [cfg.types :refer [merge-typedefs process-mixins]]
            [cfg.types.cli :refer :all]))

(defprotocol PConfig
  (add-opt [me k optdef])
  (nest [me k config]))

(deftype Config [options aliases basetype docstring]
  PConfig
  (add-opt [me k optdef]
    )
  (nest [me k config]))

(defn- rewrite-opt
  "resolves the syntactic sugar of the opt command within the config macro.
  sym is the symbol bound to the Config which the opt is being defined for"
  [sym args]
  (let [[k & more] args
        [aliases more] (split-with symbol? more)
        [mixins docstring typeargs] (get-mixins-and-docstring more)

        typedef (reduce merge (apply hash-map typeargs)
                  [{:aliases (mapv name aliases)}
                  (if docstring {:docstring docstring} {})])]

    `(add-opt ~sym ~k (reduce merge-typedefs ~typedef (process-mixins ~mixins)))))

(defn- call? [sym form]
  (and
    (list? form)
    (= sym (first form))))

(defn- traverse [sym body]
  (for [form body]
    (cond
      (call? 'opt form)
        (rewrite-opt sym (rest form))
      (call? 'opts form)
        `(nest ~sym ~(second form) (config ~@(drop 2 form)))
      (list? form)
        (traverse sym form)
      :else form)))

(defmacro config [& body]
  (let [[mixins docstring body] (get-mixins-and-docstring body)
        cfg-sym (gensym "cfg")]
    `(let [~cfg-sym (Config. {} {} (reduce merge-typedefs {} ~mixins) ~docstring)]
      ~@(traverse cfg-sym body))))

(defmacro prgood [body]
  `(clojure.pprint/pprint (macroexpand (quote ~body))))

(prgood (config "teets" [int32] (opt :something --coffee [int32 pos?] "yes")))

()
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