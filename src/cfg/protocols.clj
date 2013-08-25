(ns cfg.protocols)

(defprotocol PConfig
  (add-opt [me k optdef])
  (nest [me k config])
  (parse-cli-args [me args])
  (merge-configs [me other])
  (get-typemap [me ks]))

(defprotocol Validator
  (validate [me & args]))

(extend-type clojure.lang.IFn
  Validator
  (validate [me & args]
    (apply me args)))

(defprotocol GetWithin
  (get-within [me ks]))

(extend-type clojure.lang.IPersistentMap
  GetWithin
  (get-within [me [k & ks]]
    (when k
      (when-let [v (me k)]
        (if (satisfies? GetWithin v)
          (get-within v ks)
          (when (empty? ks) v))))))
