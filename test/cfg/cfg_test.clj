(ns cfg.cfg-test
  (:use cfg.core midje.sweet))

(init) ;this sets everything up

(defopt :foo)

(fact ":foo has been defined"
  (opt :foo) => nil)

(fact ":bar has not been defined"
  (opt :bar) => (throws IllegalArgumentException))

(defopt :bar
  :default "quux")

(fact ":bar has now been defined with a default value of 'quux'"
  (opt :bar) =>  "quux")

(fact "you can nest options within containers using `defopts`"
  (defopts :container
    (defopt :hello
      :default "world!")
    (defopt :michael
      :default "jackson!"))
  (opt :container :hello)   => "world!"
  (opt :container :michael) => "jackson!")

(fact "You can pass the container's path to `opt` to get the nested values"
  (opt :container) => {:hello "world!" :michael "jackson!"})

(fact "just `(opt)` will give you the whole config map"
  (opt) => {
      :foo nil
      :bar "quux"
      :container {
        :hello "world!"
        :michael "jackson!"
      }
    })

(fact "You can merge another map (e.g. one parsed from a config file) into the current config."
  (merge-opts! {
      :foo true
      :container { :michael "jordan?" }
    })
  (opt) => {
      :foo true
      :bar "quux"
      :container {
        :hello "world!"
        :michael "jordan?"
      }
    })

(fact "You don't need to use keywords as keys, if it's not convenient."
  (defopt "stringy"
    :default {"morestringy" true})
  (opt "stringy") => {"morestringy" true})


(fact "If you give an opt a map value, it's keys aren't options"
  (opt "stringy" "morestringy") => (throws IllegalArgumentException))

(fact "It is definitely a good idea to be consistent about your key types."
  (delopt! "stringy")
  (opt "stringy") => (throws IllegalArgumentException))

(fact "When you def an opt, you can define a function that should be used to merge new values with old values."
  (defopt :pubs-in-brighton
    :default ["king and queen" "hobgoblin"]
    :merge   into)
  (merge-opts! {:pubs-in-brighton ["great eastern" "black lion"]})

  (opt :pubs-in-brighton) => ["king and queen" "hobgoblin" "great eastern" "black lion"])

(delopt! :pubs-in-brighton)

(fact "You can parse args from the command line. Unused args get returned."
  (parse-cli-args! ["-:foo" "blah" "these" "-:container:hello" "there!" "unused"])
  => ["these" "unused"]
  (opt :foo)              => "blah"
  (opt :container :hello) => "there!")

(fact "If you're using string keys, or some other kind of keys, you can supply a key function. The default is `clojure.core/keyword`."
  (defopt "stringyagain")
  (parse-cli-args! ["-:stringyagain" "hahaha"] identity) => []
  (opt "stringyagain")                                   => "hahaha"
  (parse-cli-args! ["-:stringyagain" "hohoho"])          => (throws IllegalArgumentException))

(delopt! "stringyagain")

(fact "You can set a validation function such that an IllegalArgumentException is thrown if it returns false."
  (defopt :foo
    :validate #(.startsWith % "p"))
  (parse-cli-args! ["-:foo" "par"]) => []
  (opt :foo)                        => "par"
  (parse-cli-args! ["-:foo" "bar"]) => (throws IllegalArgumentException))

(fact "You can set a parsing function, if you want a number or to transform the argument in some way."
  (defopt :foo
    :parse    #(Integer. %)
    :validate #(>= % 5)) ; validation occurs after parsing
  (parse-cli-args! ["-:foo" "10"]) => []
  (opt :foo) => 10)

(fact "You can set `:aliases` for an option, such that users don't need to type the whole key path."
  (defopts :some
    (defopts :deeply
      (defopts :nested
        (defopt :option
          :default "i"
          :merge   str
          :aliases ["b" "-barbarblacksheep" "haveyouanywool?"]))))
  (parse-cli-args! ["-b" " love"])
  (parse-cli-args! ["--barbarblacksheep" " being"])
  (parse-cli-args! ["-haveyouanywool?" " silly"])

  (opt :some :deeply :nested :option) => "i love being silly")

(fact "You can totally use those aliases too."
  (opta "b")                 => "i love being silly"
  (opta "-barbarblacksheep") => "i love being silly"
  (opta "haveyouanywool?")   => "i love being silly")

(delopt! :some :deeply :nested :option)


(fact "It is an IllegalArgumentException if a user tries to set a private opt"
  (defopt :foo
    :private true
    :aliases ["f"])
  (parse-cli-args! ["-f" "value"]) => (throws IllegalArgumentException))

(fact "if you set both `:aliases` and `:help-string` for an option, it'll get explained when you call `print-help`."

  (defopt :foo
    :aliases     ["f" "-foo"]
    :help-string "Sets the foo option. IT IS REQUIRED!!")

  (defopt :bar
    :default     5
    :parse       #(Integer. %)
    :aliases     ["b" "-barbarblacksheep"]
    :help-string "I've got all the wool! (default 5)")

  (let [tmp (java.io.StringWriter.)]
    (binding [*out* (java.io.PrintWriter. tmp)]
      (print-help))

    (.toString tmp)
    =>
"OPTIONS
=======

    -b --barbarblacksheep
        I've got all the wool! (default 5)
    -f --foo
        Sets the foo option. IT IS REQUIRED!!
"))

(fact "If your option is a boolean flag, use `:bool true`. When the flag is given, it gets set to `(not whatever-it's-default-is)`."
  (defopt :use-karate
    :bool    true
    :aliases ["k"]) ; it'll be false (actually nil) if you don't specify default.
  (defopt :use-the-force
    :bool    true
    :default true
    :aliases ["f"])

  (parse-cli-args! ["-k" "something" "-f"]) => ["something"]
  (opt :use-karate)    => true
  (opt :use-the-force) => false)


(facts "About merging opts from other sources."
  (fact "If you're merging opts from a user-generated config file, it might be wise to use `merge-public-opts!`"
    (defopt :use-the-force
      :private true
      :parse   identity
      :default true)
    (merge-public-opts! {:use-the-force false}) => (throws IllegalArgumentException)
    (merge-opts! {:use-the-force "luke"})
    (opt :use-the-force) => "luke")

  (fact "If the opts you're merging need to be parsed, you can use `merge-unparsed-opts!` which assumes public origin."
    (defopt :midichlorians
      :parse   #(Long. %)
      :default 0)
    (merge-unparsed-opts! {:midichlorians "759465461564614634"})
    (opt :midichlorians) => 759465461564614634)

  (fact "opts get validated before they are merged."
    (defopt :funny-words
      :validate vector?
      :default  ["spiffy" "genuflect"]
      :merge    into)
    (merge-opts! { :funny-words "haha" })      => (throws IllegalArgumentException)
    (opt :funny-words)                         => ["spiffy" "genuflect"]
    (merge-opts! { :funny-words ["bajingo"] })
    (opt :funny-words)                         => ["spiffy" "genuflect" "bajingo"])

  (fact "if you just want to set an option and you don't care about any merging or validation or parsing, use `set-opt!`"
    (set-opt! [:funny-words] "there are none!")
    (opt :funny-words) => "there are none!")

  (fact "but the opt must have been def'd before you use it."
    (set-opt! [:bananas] "are yellow") => (throws IllegalArgumentException)
    (set-opt! [] "emptyness inside"))) => (throws IllegalArgumentException)


