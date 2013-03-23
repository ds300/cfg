(ns cfg.core)


(defmacro init []
  (let [data (str "(do" (slurp (clojure.java.io/resource "cfg/cfg.clj")) ")")]
    `(eval (clojure.core/read-string ~data))))

(defn dissoc-in [m [k & ks]]
  (if ks
    (if-let [child (m k)]
      (let [r (dissoc-in child ks)]
        (if (empty? r)
          (dissoc m k)
          (assoc m k r)))
      m)
    (dissoc m k)))

(defn arg-to-location [s keyfn]
  (map keyfn (re-seq #"(?<=:)[^:]+" s)))
