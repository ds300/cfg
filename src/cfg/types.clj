(ns cfg.types
  (:require [clojure.core.match :refer [match]]))

(defprotocol AbstractType
  (compose [me other])
  (finalize [me])
  (validate [me value])
  (parse [me value])
  (get-default [me]))

(defrecord MapType [properties parser validator default-getter]
  AbstractType
  (validate [me value] (validator value))
  (parse [me value] (parser value))
  (get-default [me] (default-getter)))

(defrecord SeqType [properties-list properties parser validator default-getter]
  AbstractType
  (validate [me value] (validator value))
  (parse [me value] (parser value))
  (get-default [me] (default-getter)))

(defrecord ValType [properties-list properties parser validator default-getter]
  AbstractType
  (validate [me value] (validator value))
  (parse [me value] (parser value))
  (get-default [me] (default-getter)))

(defrecord AbstractTypeProperty [])

(defn merge-type-properties [a b]
  (loop [acc a [[k v :as e] & more] (seq b)]
    (if e
      (case k
        :add-parser
          (recur (update-in acc [:parsers] (fnil conj []) v) more)
        :parse
          (recur (assoc acc :parsers [v]) more)
        :add-validator
          (recur (update-in acc [:validators] (fnil conj []) v) more)
        :validate
          (recur (assoc acc :parsers [v]) more)
        (recur (conj acc e) more))
      acc)))

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

(defn make-parser [{parsers :parsers}]
  (if (empty? parsers)
    identity
    (apply comp (reverse parsers))))

(defn make-validator [{validators :validators}]
  (if (empty? validators)
    (constantly true)
    (apply every-pred validators)))

(defn make-default-getter [{generate :gen-default default :default :as props}]
  (if generate
    generate
    (if (contains? props :default)
      (constantly default)
      (constantly nil))))


(extend-type MapType
  AbstractType
  (compose [me other]
    (cond
      (instance? MapType other)
        (->MapType (apply map-merge (map :properties [me other])) nil nil nil)
      :else (throw (Exception.
               (str "Map types are only composable with other map types"))))))

(extend-type SeqType
  AbstractType
  (compose [me other]
    (cond
      (instance? SeqType other)
        (->SeqType (apply into (map :properties-list [me other]))
          nil nil nil nil)
      (instance? ValType other)
        (->SeqType  (into (:properties-list me)
                      (concat
                        (for [validator (:validators (:properties other))]
                          (->AbstractTypeProperty {:add-validator validator}))
                        (for [parser (:parsers (:properties other))]
                          (->AbstractTypeProperty {:add-parser parser}))))
          nil nil nil nil)
      (satisfies? AbstractType other)
        (add-parser-and-validator me other)
      (instance? AbstractTypeProperty other)
        (update-in me [:properties] conj other)
      :else
        (throw (Exception. (str "Type " (type other) " is not composable.")))))

  (finalize [{properties-list :properties-list}]
    (let [properties (reduce merge-type-properties properties-list)
          parser (make-parser properties)
          validator (make-validator)]
      (->ValType
        properties-list
        properties
        #(mapv parser %)
        #(mapv validator %)
        (make-default-getter properties)))))

(extend-type ValType
  AbstractType
  (compose [me other]
    (cond
      (instance? ValType other)
        (->ValType (apply into (map :properties-list [me other]))
          nil nil nil nil)
      (satisfies? AbstractType other)
        (add-parser-and-validator me other)
      (instance? AbstractTypeProperty other)
        (update-in me [:properties] conj other)
      :else
        (throw (Exception. (str "Type " (type other) " is not composable.")))))

  (finalize [{properties-list :properties-list}]
    (let [properties (reduce merge-type-properties properties-list)]
      (->ValType (:properties-list me)
        properties
        (make-parser properties)
        (make-validator properties)
        (make-default-getter properties)))))

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
  (let [properties (->AbstractTypeProperty (apply hash-map fields))]
    (if description
      (assoc properties :description description)
      properties)))

(defn valtype [& args]
  (let [[mixins description fields] (get-mixins-and-docstring args)]
    (finalize
      (reduce compose (->ValType [] nil nil nil nil)
        (conj mixins (process-description-and-fields description fields))))))

(defn seqtype [& args]
  (let [[mixins description fields] (get-mixins-and-docstring args)]
    (finalize
      (reduce compose (->SeqType [] nil nil nil nil)
        (conj mixins (process-description-and-fields description fields))))))

(defn process-map-field [mixins field]
  (cond
    (satisfies? AbstractType field)
      field
    (vector? field)
      (finalize (valtype (into mixins field)))
    (map? field)
      (update-with (partial process-map-field mixins) field)))

(defn maptype [& args]
  (let [[mixins description fields] (get-mixins-and-docstring args)
        properties (process-description-and-fields description (butlast fields))
        base-type  (process-mixins (or (:base-type properties) []))
        structure  (update-with (partial process-map-field base-type)
                     (last (fields)))]
    (finalize
      (reduce compose (->MapType {} nil nil nil)
        (conj mixins
          (->MapType (assoc properties :structure structure) nil nil nil))))))

(valtype [mixins] "description"
  :keys :values)

(maptype
  :base-type [integer]
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
