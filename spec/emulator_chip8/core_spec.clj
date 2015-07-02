(ns emulator-chip8.core-spec
  (:require [speclj.core :refer :all]
            [emulator-chip8.core :refer :all]))


(describe "Truth"

          (it "is true"
              (should true))

          (it "is not false"
              (should-not false)))

(run-specs)
