(ns cfg.types
  (:require [clojure.core.match :refer [match]]))

(defprotocol ProtoType
  (validate [me value])
  (parse [me value])
  (get-default [me])
  (properties [me]))

(defprotocol AbstractType
  (compose [me other])
  (finalize [me]))

(defrecord AbstractMap [properties])
(defrecord AbstractSeq [properties])
(defrecord AbstractVal [properties])

(defrecord AbstractTypeProperty [])

(defn add-validator [abstype validator]
  (update-in abstype [:properties]
    conj (->AbstractTypeProperty {:add-validator validator})))

(defn add-parser [abstype parser]
  (update-in abstype [:properties]
    conj (->AbstractTypeProperty {:add-parser parser})))

(defn add-parser-and-validator [abstype-a abstype-b]
  (let [t (finalize abstype-b)]
    (-> abstype-a
      (add-validator (partial validate t))
      (add-parser    (partial parse t)))))

(defn update-with [f m]
  (into {} (for [[k v] m] [k (f v)])))

(defn map-merge [m1 m2]
  (into m1
    (for [[k v] m2]
      (if (every? map? [v (m1 k)])
        [k (map-merge v (m1 k))]
        [k v]))))

(extend-type AbstractMap
  AbstractType
  (compose [me other]
    (if (instance? AbstractMap other)
      (->AbstractMap (apply map-merge (map :properties [me other])))
      (throw (Exception.
               (str "Map types are only composable with other map types"))))))

(extend-type AbstractSeq
  AbstractType
  (compose [me other]
    (cond
      (instance? AbstractSeq other)
        (->AbstractSeq (apply into (map :properties [me other])))
      (satisfies? AbstractType other)
        (add-parser-and-validator me other)
      (instance? AbstractTypeProperty other)
        (update-in me [:properties] conj other)
      :else
        (throw (Exception. (str "Type " (type other) " is not composable."))))))

(extend-type AbstractVal
  AbstractType
  (compose [me other]
    (cond
      (instance? AbstractVal other)
        (->AbstractVal (apply into (map :properties [me other])))
      (satisfies? AbstractType other)
        (add-parser-and-validator me other)
      (instance? AbstractTypeProperty other)
        (update-in me [:properties] conj other)
      :else
        (throw (Exception. (str "Type " (type other) " is not composable."))))))

(defn process-mixins [ms]
  (vec
    (for [m ms]
      (cond
        (satisfies? AbstractType m)
          m
        (map? m)
          (->AbstractTypeProperty {:add-parser m})
        (vector? m)
          (seqtype m)
        (instance? clojure.lang.IFn m)
          (->AbstractTypeProperty {:add-validator m})
        :else
          (throw (Exception. (str "Type " (type m) " cannot be mixed in")))))))

(defn get-mixins-and-docstring [args]
  (let [[[mixins] remaining] (split-with vector? args)
        [[description] remaining] (split-with string? remaining)]
    [(or (process-mixins mixins) []) description remaining]))

(defn process-description-and-fields [description fields]
  (let [properties (apply hash-map fields)]
    (if description
      (assoc properties :description description)
      properties)))

(defn valtype [& args]
  (let [[mixins description fields] (get-mixins-and-docstring args)]
    (->AbstractVal
      (conj mixins (process-description-and-fields description fields)))))

(defn seqtype [& args]
  (let [[mixins description fields] (get-mixins-and-docstring args)]
    (->AbstractSeq
      (conj mixins (process-description-and-fields description fields)))))

(defn process-map-field [mixins field]
  (cond
    (satisfies? AbstractType field)
      (finalize field)
    (vector? field)
      (finalize (valtype (into mixins field)))
    (map? field)
      (update-with (partial process-map-field mixins) field)))

(defn maptype [& args]
  (let [[mixins description fields] (get-mixins-and-docstring args)
        properties (process-description-and-fields description (butlast fields))
        structure  (update-with (partial process-map-field mixins) (last (fields)))]
    (->AbstractMap (assoc properties :structure structure))))

(valtype [mixins] "description"
  :keys :values)

(maptype [integer]
  :allow-other-fields ifn/true
  {
    :steve (valtype [integer]
            :validate-with-fields [[:jones]]
            :validate-with-fn (fn [steve jones] (= "steve" "jones")))
    :jones {

      }})

(maptype [integer]
  :allow-other-fields true
  (field :jones [even?]
    :validate-with-fields [[:steve]])
  (fields ))
