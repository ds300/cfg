(ns cfg.types.cli
  (:require [cfg.types :refer :all]))

(defparamtype flag
  "Boolean flag"
  :default false
  :take 0
  :merge (fn [a b] (not a)))

(defparamtype int64
  "Integer"
  :parse #(Long. %))

(defparamtype int32
  "Integer"
  :parse #(Integer. %))

(defparamtype float64
  "Floating Point Number"
  :parse #(Double. %))

(defparamtype float32
  "Floating Point Number"
  :parse #(Float. %))

(defparamtype required
  :required true)

(defparamtype multi
  :take -1)

(defparamtype tuple
  :take 2)

(defparamtype triple
  :take 3)

(defparamtype url
  "URL"
  :parse #(java.net.URL. %))

