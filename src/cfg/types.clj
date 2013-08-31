(ns cfg.types
  (:require [alandipert.kahn :refer [kahn-sort]])
  (:import  cfg.java.CFGException))

(defprotocol TypeProtocol
  (validate [me value])
  (parse [me value])
  (get-default [me])
  (compose [me other])
  (finalize [me]))

(defrecord AbstractTypeProperty [])

(defn abstract-type-property [m]
  (merge (->AbstractTypeProperty) m))

(def ^:dynamic *emit-validation-errors* false)

(defn validator-wrapper [validator]
  (fn [value]
    (try
      (validator value)  
      (catch Exception e
        (if *emit-validation-errors*
          (throw e)
          false)))))

(defn throw-error [msg] (throw (CFGException. msg nil nil nil)))

(defn throw-parse-error 
  ([key-path bad-value]
    (throw-parse-error key-path bad-value nil))
  ([key-path bad-value original-exception]
    (throw
      (CFGException. "Parsing failed." key-path bad-value original-exception))))

(defn throw-validation-error
  ([key-path bad-value]
    (throw-validation-error key-path bad-value nil))
  ([key-path bad-value original-exception]
    (throw
      (CFGException. "Validation failed." key-path bad-value original-exception))))

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
  (case (count validators)
    0 (constantly true)
    1 (first validators)
    (apply every-pred validators)))

(defn make-default-getter 
  ([m] (make-default-getter m (constantly nil)))
  ([{generate :gen-default default :default :as props} otherwise]
    (if generate
      generate
      (if (contains? props :default)
        (constantly default)
        otherwise))))

(defn all-key-paths [m]
  (apply concat
    (for [[k v] m]
      (if (and (not (satisfies? TypeProtocol v)) (map? v))
        (map #(into [k] %) (all-key-paths v))
        [[k]]))))

(defn dissoc-in [m [k & ks]]
  (if ks
    (if-let [child (m k)]
      (let [r (dissoc-in child ks)]
        (if (empty? r)
          (dissoc m k)
          (assoc m k r)))
      m)
    (dissoc m k)))

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

;;;;; NEEED TO SORT OUT EXCEPTIONS LIKE NOW

(defn make-map-field-parsers [structure]
  (for [key-path (all-key-paths structure)]
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
        (try
          (f key-path m)
        (catch Exception e
          (throw-parse-error key-path (get-in m key-path) e)))))))

(defn make-map-parser [structure]
  (let [parsers (make-map-field-parsers structure)]
    (fn [value]
      (reduce 
        (fn [acc f]
          (f acc))
        value
        parsers))))

(defn make-map-field-validators [structure]
  (for [key-path (get-topological-sort structure)]
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
        (try
          (or
            (loop [m value [k & ks] key-path]
              (if (and (map? m) (contains? m k))
                (if ks
                  (recur (m k) ks)
                  (validator value (m k)))
                (not required?)))
            (throw-validation-error key-path (get-in value key-path)))
          (catch Exception e
            (throw-validation-error key-path (get-in value key-path) e)))))))

(defn make-map-validator [structure]
  (let [validators (make-map-field-validators structure)]
    (fn [value]
      (loop [[v & more] validators]
        (if v
          (if (v value)
            (recur more)
            false)
          true)))))

(defn wrap-map-validator:no-other-fields [validator key-paths]
  (fn [value]
    (if (validator value)
      (let [reduced (reduce dissoc-in value key-paths)]
        (if (empty? reduced)
          true
          (throw (Exception. (str "Unauthorized fields in map: "(all-key-paths reduced))))))
      false)))


(defn make-map-default-getter [structure]
  (let [key-paths (all-key-paths structure)]
    (fn []
      (reduce (partial apply assoc-in) {}
        (map (juxt identity #(get-default (get-in structure %))) key-paths)))))




(defrecord MapType [properties parser validator default-getter]
  TypeProtocol
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
      (let [parser (make-map-parser structure)
            validator (make-map-validator structure)
            validator (if (and (contains? properties :allow-other-fields)
                               (not (:allow-other-fields properties)))
                        (wrap-map-validator:no-other-fields validator
                          (all-key-paths structure))
                        validator)
            default-getter (make-default-getter properties
                             (make-map-default-getter structure))]
        (->MapType
          properties
          parser
          (validator-wrapper validator)
          default-getter))

      (->MapType
        properties
        (partial update-with (partial parse (:base-type properties)))
        #(every? (partial validate (:base-type properties)) (keys %))
        (constantly {})))))


(defrecord ValType [properties-list properties parser validator default-getter]
  TypeProtocol
  (validate [me value] (validator value))
  (parse [me value] (parser value))
  (get-default [me] (default-getter))

  (compose [me other]
    (cond
      (instance? ValType other)
        (->ValType (apply into (map :properties-list [me other]))
          nil nil nil nil)
      (satisfies? TypeProtocol other)
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
  TypeProtocol
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
      (satisfies? TypeProtocol other)
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
        (validator-wrapper #(and (sequential? %) (every? validator %)))
        (make-default-getter properties)))))


(declare seqtype)

(defn process-mixins [ms]
  (vec
    (for [m ms]
      (cond
        (satisfies? TypeProtocol m)
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
    (satisfies? TypeProtocol field)
      field
    (vector? field)
      (finalize (valtype (into mixins field)))
    (map? field)
      (update-with (partial process-map-field mixins) field)))

(defn maptype [& args]
  (let [[mixins description fields] (get-mixins-and-docstring args)
        properties (process-description-and-fields description fields)
        base-type  (process-mixins (or (:base-type properties) []))
        structure  (when-let [structure (:structure properties)]
                     (update-with (partial process-map-field base-type)
                       structure))
        properties (if structure
                     (assoc properties :structure structure)
                     properties)]
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

(def-valtype string [string?])

(def-maptype tweet
  :allow-other-fields false
  :structure
  {
    :text [string]
    :tokens [[string]]
    :id (valtype [integer] :required true)
    })

(validate tweet {:cheese false :text "scrotii are fun to poke" :id 7 :tokens ["yo"]})
