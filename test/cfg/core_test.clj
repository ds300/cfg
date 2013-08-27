(ns cfg.core-test
  (:require [cfg.core :refer [defconfig prgood parse-cli-args]]
            [cfg.types :refer :all]
            [cfg.protocols :refer :all]

            :reload-all))

; (prgood

(defconfig michael
  (opt :surname -s --surname)
  (opt :nicknames -n --nicknames [multi])
  (opt :age -a --age [integral even?]))

; )
(def teeth nil)

(parse-cli-args michael ["nothing" "-s" "jackson" "more nothing" "-n" "king of pop" "michael fucking jackson" "-a" "45"])