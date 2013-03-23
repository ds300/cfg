(def ALIASES (atom {}))

(def OPTS (atom {}))

(def OPTS_META (atom {}))

(defmacro defopt- [ks & args]
  (case (mod (count args) 2)
    0 (let [m (into {} (map vec (partition-all 2 args)))]
        `(do
          (clojure.core/when-let [as# (:aliases ~m)]
            (doseq [a# as#]
              (clojure.core/swap! ALIASES clojure.core/conj [a# ~ks])))
          (clojure.core/swap! OPTS clojure.core/assoc-in ~ks (:default ~m))
          (clojure.core/swap! OPTS_META clojure.core/assoc-in ~ks ~m)))
    (throw (IllegalArgumentException. "defopt requires an odd number of arguments."))))

(defmacro defopt
  "defines an option. args can be any, or none, of the following:

  :default  x        Sets the inital value of the opt to value
  :private  x        If x is truthy, sets this option as private
  :bool     x        If x is truthy, sets this option as a boolean flag
  :validate fn       fn is called when an attempt to set the value of this opt is made.
                     an IllegalArgumentException is thrown if it returns a falsey value.
  :parse    fn       The return value of (fn x) is used as this opt's value, 
                     where x is a string given at the command line.
  :merge    fn       The return value of (fn old new) is used as this opt's value.
  :aliases  [& strs] Each str becomes an alias which, when prefixed with \\-, can be
                     used to set this opt at the command line.
  :help-string s     When :aliases is also set, this is printed when `print-help` is invoked."
  [k & args]
  `(defopt- [~k] ~@args))

(defmacro defopts- [ks & args]
  `(do 
    ~@(for [[cmd k & things] args]
        `(~(symbol (str (name cmd) "-")) ~(conj ks k) ~@things))))

(defmacro defopts
  "define an option container. args should be a list of `defopt` or `defopts` calls"
  [k & args]
  `(defopts- [~k] ~@args))


(defn delopt!
  "delete the option at ks"
  [& ks]
  (when-let [m (get-in @OPTS_META ks)]
    (when-let [as (:aliases m)]
      (doseq [a as]
        (swap! ALIASES dissoc a)))
    (swap! OPTS_META dissoc-in ks)
    (swap! OPTS dissoc-in ks)))

(defn opt
  "Gets an option, or a map of options. throws IllegalArgumentException if not defined."
  [& ks]
  (if (get-in @OPTS_META ks)
    (get-in @OPTS ks)
    (throw (IllegalArgumentException. (str "No cfg option at " ks)))))

(defn opta
  "get an option by passing in its cli alias"
  [a]
  (if-let [ks (@ALIASES a)]
    (apply opt ks)
    (throw (IllegalArgumentException. (str "No cfg option with alias " a)))))



(defn parse-cli-args!
  "parses options from the command line, validating them,
  and including them in the options map. Returns unused args."
  ([args] (parse-cli-args! args keyword))
  ([[id & [v & more :as others]] keyfn]
    (let [fail #(throw (IllegalArgumentException. (str "Invalid or private option: " id)))
          do-stuff  (fn [loc]
                      (if-let [m (get-in @OPTS_META loc)]
                        (do
                          (when (:private m) (fail))
                          (let [bool (:bool m)
                                parse (or (:parse m) identity)
                                merge (or (:merge m) (fn [a b] b))
                                validate (or (:validate m) (fn [a] true))
                                ensure (fn [a]
                                          (if (validate a)
                                            a
                                            (throw
                                              (IllegalArgumentException.
                                                (str "Option " id  " failed validation with a value of '" v "'")))))]
                            (if bool
                              (do
                                (swap! OPTS update-in loc not)
                                (parse-cli-args! others))
                              (do
                                (swap! OPTS assoc-in loc (merge (apply opt loc) (ensure (parse v))))
                                (parse-cli-args! more)))))
                        (fail)))]
      (case (first id)
        nil ()
        \-  (if (= \: (second id))
              (do-stuff (arg-to-location (apply str (rest id)) keyfn))
              (if-let [loc (@ALIASES (apply str (rest id)))]
                (do-stuff loc)
                (fail)))
        (cons id (parse-cli-args! others))))))

(defn merge-opts-! [loc m public? parse?]
    (if (map? m)
      (doseq [[k v] (seq m)]
        (merge-opts-! (conj loc k) v public? parse?))
      (let [meta (get-in @OPTS_META loc)
            private (:private meta)
            merge-fn  (or (:merge meta) (fn [a b] b))
            validate  (or (:validate meta) (fn [a] true))
            parse     (if parse? (:parse meta identity) identity)
            ensure    (fn [a]
                        (if (validate a)
                          a
                          (throw
                            (IllegalArgumentException.
                              (str "Failed to merge config map. Validation of field " loc " failed with value " m)))))]
        (if meta 
          (if-not (and public? private)
            (swap! OPTS assoc-in loc (merge-fn (apply opt loc) (ensure (parse m))))
            (throw (IllegalArgumentException. (str "Unable to set private field: " loc))))
          (throw (IllegalArgumentException. (str "Failed to merge config map. Invalid field: " loc)))))))
  
(defn merge-opts!
  "Merges option map with the given one, presumably from a trusted source"
  ([loc m] (merge-opts-! loc m false false))
  ([m]     (merge-opts-! [] m false false)))

(defn merge-public-opts!
  "Merges option map with the given one, disallowing modification of private options"
  ([loc m] (merge-opts-! loc m true false))
  ([m]     (merge-opts-! [] m true false)))

(defn merge-unparsed-opts!
  "Merges option map with the given one, parsing options first, and disallowing modification of private options"
  ([loc m] (merge-opts-! loc m true true))
  ([m]     (merge-opts-! [] m true true)))

(defn set-opt!
  "Sets the option at ks to v"
  [ks v]
  (if (get-in @OPTS_META ks)
    (swap! OPTS assoc-in ks v)
    (throw (IllegalArgumentException. (str "No cfg option at " ks)))))

(defn print-help
  "Call without arguments. Prints the set of options which have both help strings and cli aliases"
  ([] (println "OPTIONS\n=======\n") (print-help @OPTS_META))
  ([m]
    (when (map? m)
      (doseq [v (vals m)]
        (let [help (:help-string v)
              aliases (:aliases v)]
          (when (and help aliases)
            (print "    ")
            (apply println (map #(str "-" %) aliases))
            (println "       " help)))
        (print-help v)))))

