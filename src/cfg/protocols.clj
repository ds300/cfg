(ns cfg.protocols)

(defprotocol PConfig
  (validate [me data])
  (parse [me data]))

(defprotocol GetWithin
  (get-within [me ks]))

(extend-type clojure.lang.IPersistentMap
  GetWithin
  (get-within [me [k & ks]]
    (when k
      (when-let [v (get me k)]
        (if (empty? ks)
          v
          (when (satisfies? GetWithin v)
            (get-within v ks)))))))

