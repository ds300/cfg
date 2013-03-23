(ns cfg.core-test
  (:use midje.sweet cfg.core))

(facts "about `dissoc-in`"
  (fact "it removes elements from a nested map structure"
    (dissoc-in {:a {:b {:d true} :c true}} [:a :c])
    => {:a {:b {:d true}}})
  (fact "it removes empty maps too."
    (dissoc-in {:a {:b {:d true} :c true}} [:a :b :d])
    => {:a {:c true}}))

(fact "`arg-to-location` takes a colon-delimited string, such as :blah:hello:sir, and returns a seq of keyfn applied to the args"
  (arg-to-location ":hello:there:sir" keyword) => [:hello :there :sir]
  (arg-to-location ":hello:there:sir:" identity) => ["hello" "there" "sir"]
  (arg-to-location ":easy:bruv::::" keyword) => [:easy :bruv])

