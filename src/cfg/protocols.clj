(ns cfg.protocols)

(defprotocol PConfig
  (add-opt [me k optdef])
  (nest [me k config])
  (parse-args [me args]))

(defprotocol Validator
  (validate [me & args]))

(extend-type clojure.lang.IFn
  Validator
  (validate [me & args]
    (apply me args)))
