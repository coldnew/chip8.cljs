(ns emulator-chip8.opcode-spec
  (:require [speclj.core :refer :all]
            [emulator-chip8.opcode :refer :all]))

(describe "opcode spec"

          (it "Should has 35 opcode"
              (should= 35 (count @opcode-list)))

          (it "Store number NN in register VX."
              (should= 234
                       (:PC (opcode-1NNN {:PC 0} {:NNN 234})

                            ))
              ))
