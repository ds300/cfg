(ns cfg.core-test
  (:require [cfg.core :refer [defconfig prgood parse-cli-args get-defaults all-keys]]
            [cfg.types :refer :all]
            [cfg.protocols :refer :all]

            :reload-all))

; (prgood

(defconfig michael
  (opt :surname -s --surname :default "jordan"
    :validate-with [:nicknames]
    :external-validate (fn [surname nicknames] (contains? nicknames surname)))
  (opt :nicknames -n --nicknames [multi])
  (opts :vitals
    (opt :mollusc-preference :default "true"))
  (opt :age -a --age [integral even?]))

(config
  :surname)

(defn build-validation-queue [conf]
  (loop [[[ks ds] :as deps] (get-validation-dependencies conf)
         queue []]
    (if ks
      (if ds
        (if (every? (partial contains? queue) ds)
          (recur deps (conj queue ks))
          (recur (concat deps [[ks ds]]) queue))
        (recur deps (conj queue ks)))
      queue)))

(defn get-validation-dependencies [conf]
  (map (juxt identity #(get-in (:options conf) (conj % :validate-with)))
    (all-keys conf)))

(get-in michael [:options :surname :validate-with])

(build-validation-queue michael)

(get-validation-dependencies michael)

(defn build-dependency-graph [deps]
  )



(satisfies? PConfig michael)

(type (get-in michael [:options :vitals]))

(into [] (map (partial into [:hello]) (satisfies? PConfig (get-in michael [:options :vitals])))))
; )
(clojure.pprint/pprint michael)