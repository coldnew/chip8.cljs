#?(:cljs (remove-ns 'cljs.analyzer))

(ns emulator-chip8.opcode-spec
  #?(:cljs
     (:require-macros [specljs.core :refer [describe it should= run-specs]]))
  (:require
   #?(:clj [speclj.core :refer :all] :cljs [speclj.core])
   #?(:clj [emulator-chip8.opcode :refer :all]
           :cljs [emulator-chip8.opcode]
           )
   ))

#?(:cljs (require '[cljs.analyzer]))

(describe "opcode spec"

          (it "Should has 35 opcode"
              (should= 35 (count @opcode-list)))

          (it "Store number NN in register VX."
              (should= 234
                       (:PC (opcode-1NNN {:PC 0} {:NNN 234})

                            ))
              ))
