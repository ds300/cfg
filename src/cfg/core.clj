(ns cfg.core)

(defprotocol PConfig
  (add-opt [me optdef]))

(deftype Config [options aliases]
  PConfig
  (add-opt [me optdef]
    ))

(defmacro log [s]
  `(println  (str ~(name s) ": " (pr-str ~s))))

(def pooh "hey poohface")
(log pooh)

(defn- rewrite-opt-args [& args]
  (let [[k & more] args
        [aliases more] (split-with symbol? more)
        [[mixins] more] (split-with vector? more)
        [[docstring] typeargs] (split-with string? more)
        ]
    (log k)
    (log aliases)
    (log mixins)
    (log docstring)
    (log typeargs)))

(defn- call? [sym form]
  (and
    (list? form)
    (= sym (first form))))

(defn- rewrite-opts [sym & args]
  `(let [cfg# (Config. {} {})]
    blah))

(defn- traverse [sym body]
  (for [form body]
    (cond
      (call? 'opt form)
        `(add-opt sym ~@(rewrite-opt-args (rest form)))
      (call? 'opts form)
        `(let [cfg# (Config. {} {})]
          (nest sym )))))


(defmacro config [& body]
  `(let [cfg# (Config. {} {})]
    ~@(for [form body]
        (if (#{'opt} (first form))
          ()
          form))))

(defmacro opt [& args]
  (apply rewrite-opt-args args))

(defconfig myconfig
  (opt :overwrite -r --overwrite [flag]
    "Choose whether or not to overwrite the specified output file"
    )
  (opt :outpath -o --output-path
    [thing etc]
    "the output path"
    :validate-with :overwrite
    :validate-with-fn (fn [f overwrite?] (if (.exists (as-file f))))))
