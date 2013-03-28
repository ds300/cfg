

(def ^:private OPT_TYPES (atom {}))



(defn legit-property? [[k v]]
  (case k
    :parse (fn? v)
    :validate (fn? v)
    :aliases (and (vector? v) (every? string? v))
    :take (integer? v)
    :take-while (fn? v)
    :merge (fn? v)
    :description (string? v)
    :private true
    :default true
    false))

(defn ensure-legit-property [p]
  (if (legit-property? p)
    p
    (throw (IllegalArgumentException. "Illegal property definition."))))

(defmacro defopttype [tsym & args]
  (let [[mixins properties] (split-with symbol? args)]
    (when-not (even? (count properties))
      (throw (IllegalArgumentException. "defopttype expects an even number of forms")))
    (when-not (or (empty? mixins) (= '< (first mixins)))
      (throw (IllegalArgumentException. "malformed defopttype. Did you forget the < ?")))

    (when-let [bad (seq (filter (complement @OPT_TYPES) (rest mixins)))]
      (throw (IllegalArgumentException. (str "Undefined opttype(s): " bad))))
    (let [properties (into {}
                       (map (comp ensure-legit-property eval vec)
                         (partition 2 properties)))]
      (swap! OPT_TYPES assoc tsym
        (merge
          (reduce merge {} (map @OPT_TYPES (next mixins)))
          properties))))
  true)


(defopttype int
  :take 1
  :parse #(Integer. %)
  :description "integer")

(prn @OPT_TYPES)

(defopttype uint < int
  :validate #(>= % 0)
  :description "integer >= 0")

(defopttype natint < int
  :validate #(> % 0)
  :description "integer > 0")

(defopttype flag
  :take 0
  :merge (fn [a b] (not a)))

