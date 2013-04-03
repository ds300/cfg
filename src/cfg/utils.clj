(ns cfg.utils)

(defn dissoc-in [m [k & ks]]
  (if ks
    (if-let [child (m k)]
      (let [r (dissoc-in child ks)]
        (if (empty? r)
          (dissoc m k)
          (assoc m k r)))
      m)
    (dissoc m k)))

(defn fail! [& args]
  (throw (IllegalArgumentException. (apply str args))))

(defmacro fail-when [test & args]
  `(clojure.core/when ~test
    (fail! ~@args)))

(defmacro fail-when-let [bind & args]
  `(clojure.core/when-let ~bind
    (fail! ~@args)))

(defmacro fail-when-not [test & args]
  `(fail-when (clojure.core/not ~test) ~@args))

(defn por
  "Predicate or. Returns a fn which takes some args and applies
   fs to them. When one returns truthy, true is returned. Otherwise
   false is returned."
  [& fs]
  (fn [& args]
    (loop [[f & more] fs]
      (if f
        (if (apply f args)
          true
          (recur more))
        false))))

(defn pand
  "Predicate and."
  [& fs]
  (fn [& args]
    (loop [[f & more] fs]
      (if f
        (if (apply f args)
          (recur more)
          false)
        true))))

(defn take-while-unthrown [pred [head & tail :as coll]]
  (when (seq coll)
    (try
      (if (pred head)
        (cons head (lazy-seq (take-while-unthrown pred tail)))
        nil)
      (catch Exception e nil))))
