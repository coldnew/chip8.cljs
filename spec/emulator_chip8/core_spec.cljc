#?(:cljs (remove-ns 'cljs.analyzer))

(ns emulator-chip8.core-spec
  #?(:cljs
     (:require-macros [specljs.core :refer [describe it should= run-specs]]))
  (:require
   #?(:clj [speclj.core :refer :all] :cljs [speclj.core])
   ;;[emulator-chip8.core :refer [] ]
   ))

#?(:cljs (require '[cljs.analyzer]))

(describe "Truth"

          (it "is true"
              (should true))

          (it "is not false"
              (should-not false)))

(run-specs)
