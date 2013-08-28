(ns cfg.types
  (:require [clojure.core.match :refer [match]]))

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

(defn update-with [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn get-mixins-and-docstring [args]
  (let [[[mixins] remaining] (split-with vector? args)
        [[description] remaining] (split-with string? remaining)]
    [(or mixins []) description remaining]))

(defn process-description-and-fields [description fields]
  (let [properties (apply hash-map fields)]
    (if description
      (assoc properties :description description)
      properties)))

(defn valtype [& args]
  (let [[mixins description fields] (get-mixins-and-docstring args)]
    (->AbstractVal (conj mixins (process-description-and-fields description fields)))))

(defn seqtype [& args]
  (let [[mixins description fields] (get-mixins-and-docstring args)]
    (->AbstractSeq (conj mixins (process-description-and-fields description fields)))))

(defn process-map-field [field]
  (finalize
    (cond
      (satisfies? AbstractType field)
        field
      (vector? field)
        (valtype field)
      (map? field)
        (maptype field))))

(defn maptype [& args]
  (let [[mixins description fields] (get-mixins-and-docstring args)
        properties (apply hash-map (butlast fields))
        structure  (update-with process-map-field (last (fields)))]))

(valtype [mixins] "description"
  :keys :values)

(maptype [integer]
  :allow-other-fields ifn/true
  {:steve [even?]})