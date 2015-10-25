(ns chip8.test-runner
  (:require
   [cljs.test :refer-macros [run-tests]]
   [chip8.cpu-test]))

(enable-console-print!)

(defn runner []
  (if (cljs.test/successful?
       (run-tests
        'chip8.cpu-test))
    0
    1))
