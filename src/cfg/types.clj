(ns cfg.types
  (:require [clojure.core.match :refer [match]]
            [alandipert.kahn :refer [kahn-sort]]))

(defprotocol AbstractType
  (validate [me value])
  (parse [me value])
  (get-default [me])
  (compose [me other])
  (finalize [me]))

(defrecord AbstractTypeProperty [])

(defn abstract-type-property [m]
  (merge (->AbstractTypeProperty) m))

(def ^:dynamic *wrap-validators* true)

(defn validator-wrapper [f]
  (if *wrap-validators*
    (fn [value]
      (try
        (f value)
        (catch Exception e false)))
    f))

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
  (update-in abstype [:properties-list]
    conj (abstract-type-property {:add-validator validator})))

(defn add-parser [abstype parser]
  (update-in abstype [:properties-list]
    conj (abstract-type-property {:add-parser parser})))

(defn add-parser-and-validator [type-a type-b]
  (-> type-a
    (add-validator (partial validate type-b))
    (add-parser    (partial parse type-b))))

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

(defn all-key-paths [m]
  (apply concat
    (for [[k v] m]
      (if (and (not (satisfies? AbstractType v)) (map? v))
        (map #(into [k] %) (all-key-paths v))
        [[k]]))))

(defn make-graph [structure]
  (into {}
    (for [ks (all-key-paths structure)]
      (let [withs (get-in structure
                    (into ks [:properties :validate-with-fields]))]
      (if (empty? withs)
        [ks #{}]
        [ks (into #{} withs)])))))

(defn get-topological-sort [structure]
  (let [graph (make-graph structure)
        sorted (kahn-sort graph)]
    (if sorted
      (reverse sorted)
      (throw (Exception. "Cyclical dependency detected")))))

(defn make-map-field-parsers [structure key-paths]
  (for [key-path key-paths]
    (let [field-type (get-in structure key-path)
          parser (partial parse field-type)
          f (fn f [[k & ks] m]
              (if k ;; probably unneccesary. do a check dave
                (if (and (map? m) (contains? m k))
                  (if ks
                    (assoc m k (f ks (m k)))
                    (assoc m k (parser (m k))))
                  m)
                m))]
      (fn [m]
        (f key-path m)))))

(defn make-map-field-validators [structure key-paths]
  (for [key-path key-paths]
    (let [field-type (get-in structure key-path)
          field-properties (:properties field-type)
          cohort-fields (:validate-with-fields field-properties)
          cohort-fn (:validate-with-fn field-properties)
          required? (:required field-properties)
          validator (if (empty? cohort-fields) 
                      (fn [map-value field-value]
                        (validate field-type field-value))
                      (fn [map-value field-value]
                        (and
                          (validate field-type field-value)
                          (apply cohort-fn
                            (map (partial get-in map-value) cohort-fields)))))]
      (fn [value]
        (loop [m value [k & ks] key-path]
          (if (and (map? m) (contains? m k))
            (if ks
              (recur (m k) ks)
              (validate field-type (m k)))
            (not required?)))))))

(defrecord MapType [properties parser validator default-getter]
  AbstractType
  (validate [me value] (validator value))
  (parse [me value] (parser value))
  (get-default [me] (default-getter))

  (compose [me other]
    (cond
      (instance? MapType other)
        (->MapType (apply map-merge (map :properties [me other])) nil nil nil)
      (instance? AbstractTypeProperty other)
        (->MapType (merge (:properties me) other) nil nil nil)
      :else
        (throw (Exception.
                 (str "Map types are only composable with other map types")))))

  (finalize [me]
    (if-let [structure (:structure properties)]
      (let [sorted-keys (get-topological-sort structure)
            parsers  (make-map-field-parsers structure sorted-keys)
            parser #(reduce (fn [value f] (f value)) % parsers)
            validators (make-map-field-validators structure sorted-keys)]
        )

      (->MapType
        properties
        (partial update-with (partial parse (:base-type properties)))
        #(every? (partial validate (:base-type properties)) (keys %))
        (constantly {})))))


(defrecord ValType [properties-list properties parser validator default-getter]
  AbstractType
  (validate [me value] (validator value))
  (parse [me value] (parser value))
  (get-default [me] (default-getter))

  (compose [me other]
    (cond
      (instance? ValType other)
        (->ValType (apply into (map :properties-list [me other]))
          nil nil nil nil)
      (satisfies? AbstractType other)
        (add-parser-and-validator me other)
      (instance? AbstractTypeProperty other)
        (update-in me [:properties-list] conj other)
      :else
        (throw (Exception. (str "Type " (type other) " is not composable.")))))


  (finalize [me]
    (let [properties (reduce merge-type-properties {} properties-list)]
      (->ValType properties-list
        properties
        (make-parser properties)
        (validator-wrapper (make-validator properties))
        (make-default-getter properties)))))

(defrecord SeqType [properties-list properties parser validator default-getter]
  AbstractType
  (validate [me value] (validator value))
  (parse [me value] (parser value))
  (get-default [me] (default-getter))


  (compose [me other]
    (cond
      (instance? ValType other)
        (->SeqType  (into (:properties-list me)
                      (concat
                        (for [validator (:validators (:properties other))]
                          (abstract-type-property {:add-validator validator}))
                        (for [parser (:parsers (:properties other))]
                          (abstract-type-property {:add-parser parser}))))
          nil nil nil nil)
      (satisfies? AbstractType other)
        (add-parser-and-validator me other)
      (instance? AbstractTypeProperty other)
        (update-in me [:properties-list] conj other)
      :else
        (throw (Exception. (str "Type " (type other) " is not composable.")))))


  (finalize [{properties-list :properties-list}]
    (let [properties (reduce merge-type-properties {} properties-list)
          parser (make-parser properties)
          validator (make-validator properties)]
      (->SeqType
        properties-list
        properties
        #(mapv parser %)
        (validator-wrapper #(every? validator %))
        (make-default-getter properties)))))


(declare seqtype)

(defn process-mixins [ms]
  (vec
    (for [m ms]
      (cond
        (satisfies? AbstractType m)
          m
        (map? m)
          (abstract-type-property {:add-parser m})
        (vector? m)
          (seqtype m)
        (instance? clojure.lang.IFn m)
          (abstract-type-property {:add-validator m})
        :else
          (throw (Exception. (str "Type " (type m) " cannot be mixed in")))))))

(defn get-mixins-and-docstring [args]
  (let [[[mixins] remaining] (split-with vector? args)
        [[description] remaining] (split-with string? remaining)]
    [(or (process-mixins mixins) []) description remaining]))

(defn process-description-and-fields [description fields]
  (let [properties (abstract-type-property (apply hash-map fields))]
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

(defmacro def-valtype [nm & args]
  `(def ~nm (valtype ~@args)))

(defmacro def-maptype [nm & args]
  `(def ~nm (maptype ~@args)))

(defmacro def-seqtype [nm & args]
  `(def ~nm (seqtype ~@args)))

(def-valtype integer [integer?]
  :parse  (fn [x]
            (if (integer? x)
              x
              (Integer. x))))

(def-valtype even-int [integer even?])

(def-seqtype int-seq [integer])

(def-valtype matrix [[[integer]]])

(validate matrix [[3] [4 5 6] [7 8 9]])

(def-valtype csv
  :parse #(clojure.string/split % #","))

(parse int-seq '(3 "3" 5 6 7))

(def-valtype csv-int [csv [integer]])

(clojure.pprint/pprint
  csv-int)

(validate csv-int (parse csv-int "3,4,5"))
; (valtype [mixins] "description"
;   :keys :values)

; (maptype
;   :base-type [integer]
;   :allow-other-fields ifn/true
;   :structure
;   {
;     :steve (valtype [integer]
;             :validate-with-fields [[:jones]]
;             :validate-with-fn (fn [steve jones] (= "steve" "jones")))
;     :jones [integer {
;         :validate-with-fields
;         :validate-with-fn
;       }]})

; (maptype [integer]
;   :allow-other-fields true
;   (field :jones [even?]
;     :validate-with-fields [[:steve]])
;   (fields ))
