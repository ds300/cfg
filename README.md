# cfg

Manage your options.

## Features

- It's epic good at command line args.
- If you've got config files, just load them into a map and merge them.
- Declare options public or private, i.e. whether or not the end-user is able to modify them.
- Define parsers, validators, and mergers for individual options.
- It throws exceptions when people do stupid things.
- If your project is super-duper complex, set up individual config namespaces for different parts of it.
- It's probably the best Clojure library that does exactly what it does.

## Usage

lein `[org.dave/cfg "1.0.0"]`

## Tutorial

The [Midje](https://github.com/marick/Midje) [test suite](http://github.com/ds300/cfg/tree/master/test/cfg/cfg_test.clj) reads much like a tutorial.

## Quick Example

Make a separate namespace for handling all your config needs

    (ns my-proj.cfg
      (:use cfg.core))
    
    (init) ; this just evals src/cfg/cfg.clj in my-proj.cfg
    
    
Now you can define you option tree with `defopt` and `defopts`
    
    (defopt :num-cats
      :default     50
      :parse       #(Long. %)  ; this gets called when parsing options from command line
      :validate    #(>= % 1)   ; IllegalArgumentException when this returns false
      :help-string "The number of cats to use."  ; this gets used by the fn print-help
      :aliases     ["c" "-cats"]) ; causes -c or --cats to be used as unix-style cli args.
    
`defopts` lets you nest options inside a container map
    
    (defopts :data-paths
    
      (defopt :dictionary
        :default  "/usr/share/dict/words"
        :validate #(.isFile (java.io.File. %)) 
        :aliases  ["d" "-dict"]
        :help-string "Where the dictionary at?")
    
      (defopt :cat-photo-dir
        ; you don't need to specify a default
        :private true)) ; means that this opt can't be set when parsing cli args.

To specify that an option should be a boolean flag, use `:bool true`
    
    (defopt :use-catnip
      :bool    true
      :default false
      :aliases ["n" "-catnip"]
      :help-string "Enables happy mode.")
      
If the user specifies `-n`, `:use-catnip` gets set to `(not `whatever it currently is`)`.

You can also specify that options should be merged.
    
    (defopt :cat-names
      :default ["muggins" "felix"]
      :parse   #(clojure.string/split % #",")
      :merge   into)
      
The merge funciton gets called with the current value first.
    
All of this translates to:

    {
      :data-paths {
        :dictionary "/usr/share/dict/words"
        :cat-photo-dir nil
      }
      :num-cats   50
      :use-catnip false
      :cat-names  ["muggins" "felix"]
    }

Now `require` the namespace you just created
    
    (ns my-proj.core
      (:require [my-proj.cfg :as cfg]))
      
    (defn usage []
      (println "USAGE: ")
      (println "    lein run <in_path> [options]\n")
      (cfg/print-help)
      (System/exit 0))
    
you can merge a config file at any point.

    (cfg/merge-opts! {:cat-names ["sooty" "bilbo"]})
    
`parse-cli-args!` does all the hard work.
    
    (defn -main [& args]
      (let [[in_path & more?] (try (cfg/parse-cli-args! args)
                                (catch IllegalArgumentException e (do (println e) (usage))))]
        (when (or more? (not in_path))
          (usage))
        (println "taking data from:"         in_path)
        (println "my dictionary is here:"    (cfg/opt :data-paths :dictionary))
        (println "numer of cats being used:" (cfg/opt :num-cats))
        (println "using catnip:"             (true? (cfg/opt :use-catnip)))
        (println "relevant names:"           (cfg/opt :cat-names))))
        
Sample output:

    (-main "-n" "path/to/data" "-:cat-names" "rex,fido,rover" "--cats" "503472")
    
    stdout=> taking data from: path/to/data
             my dictionary is here: /usr/share/dict/words
             numer of cats being used: 503472
             using catnip: true
             relevant names: [mittens felix sooty bilbo rex fido rover]
    
    (-main "-f" "undefined alias")
    
    stdout => java.lang.IllegalArgumentException: Invalid or private option: -f
              USAGE: 
                  lein run <in_path> [options]
  
              OPTIONS
              =======
  
                  -n --catnip
                      Enables happy mode.
                  -d --dict
                      Where the dictionary at?
                  -c --cats
                      The number of cats to use.

## API

`defopt [k & args]`

      defines an option. args can be any, or none, of the following:

      :default  x        Sets the inital value of the opt to value
      :private  x        If x is truthy, sets this option as private
      :bool     x        If x is truthy, sets this option as a boolean flag
      :validate fn       fn is called when an attempt to set the value of this opt is made.
                         an IllegalArgumentException is thrown if it returns a falsey value.
      :parse    fn       The return value of (fn x) is used as this opt's value, 
                         where x is a string given at the command line.
      :merge    fn       The return value of (fn old new) is used as this opt's value.
      :aliases  [& strs] Each str becomes an alias which, when prefixed with -, can be
                         used to set this opt at the command line.
      :help-string s     When :aliases is also set, this is printed when `print-help` is invoked.

`defopts [k & args]`

      defines an option container. args should be a list of `defopt` or `defopts` calls

`delopt! [& ks]`

      deletes the option at ks

`opt [& ks]`

      gets the option at ks. Throws IllegalArgumentException if it doesn't exist.

`opta [alias]`

      gets the option with the given alias. Throws IllegalArgumentException if it doesn't exist.

`parse-cli-args! [args] [args keyfn]`

      parses options from the command line. keyfn is used to process options passed as key-paths.
      i.e. [-:some:value blah] with keyfn identity results in {"some" {"value" "blah"}}. The
      default keyfn is clojure.core/keyword.

      Unused args are returned. Throws IllegalArgumentException if anything goes wrong, including:
        - failed validation
        - trying to set a private opt
        - trying to set a non-existent opt

`merge-opts! [m] [ks m]`

    Merges option map with the given m, presumably from a trusted source


`merge-public-opts! [m] [ks m]`

      Merges option map with the given one, disallowing modification of private options

`merge-unparsed-opts! [m] [ks m]`

      Merges option map with the given one, parsing options first, and disallowing modification of private options"

`set-opt! [ks v]`

      Sets the option at ks to v
      

`print-help []`

      Call without arguments. Prints the set of options which have both help strings and cli aliases


### Todo
- Atomic merges
- Allow other expressions inside `defopts`
- Change `:help-string` to `:help`
- Option 'types' e.g. flag, int, uint, nint, doubles, csv, custom.
- Throw exceptions for invalid defopt args incl. alias re-use.
- More subtle control over args parsing, e.g. take-while, take-n.

## License

Copyright © 2013 David Sheldrick

Distributed under the Eclipse Public License, the same as Clojure.