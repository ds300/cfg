(ns cfg.core)


(defmacro init []
  (let [data (str "(do" (slurp (clojure.java.io/resource "cfg/cfg.clj")) ")")]
    `(eval (clojure.core/read-string ~data))))

