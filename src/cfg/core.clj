(ns cfg.core
  (:require [cfg.utils :refer :all]
            [cfg.protocols :refer :all]
            [cfg.types :refer :all]
            :reload-all))



(defrecord Config [options aliases basetype docstring]
  GetWithin

  (get-within [me ks]
    (get-within options ks))

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

  (get-defaults [me]
    (into {}
      (filter identity
        (for [[k v] options]
        (if (satisfies? PConfig v)
          [k (get-defaults v)]
          (try [k ((:get-default k))] (catch Exception e nil)))))))

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
        (if (is-cli-opt-flag? a)
          (if-let [ks (aliases a)]
            (let [typemap (get-within me ks)
                  parse   (:cli-parse typemap)]
              (try
                (let [[value remaining] (parse things)]
                  (recur remaining errors (assoc-in acc ks value)))
                (catch Exception e
                  (recur [] (conj errors (str e)) nil nil))))
            (recur things (conj errors (str a " is not an option"))))
          (recur things errors acc (conj unused a)))
        [(and acc (parse-and-validate me acc)) errors unused])))


  (parse-and-validate [me data]
    (when data
      ))

  (validate [me data])

  (parse-only [me data]
    ))


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

