(ns cfg.types)

(defprotocol ProtoType
  (validate [me value])
  (parse [me value])
  (get-default [me]))

(defprotocol AbstractType
  (compose [me other])
  (finalize [me]))

(defrecord AbstractMap [properties])
(defrecord AbstractSeq [properties])
(defrecord AbstractVal [properties])

(extend-type AbstractMap
  AbstractType
  (compose [me other]
    (if (= AbstractMap (type other))
      (->AbstractMap (apply merge (map :properties [me other]))))))

(defn get-mixins-and-docstring [args]
  (let [[[mixins] remaining] (split-with vector? args)
        [[description] remaining] (split-with string? remaining)]
    [(or mixins []) description remaining]))

(defn valtype [& args]
  (let [[mixins description remaining] (get-mixins-and-docstring args)]
    ))

(valtype [mixins] "description"
  :keys :values)