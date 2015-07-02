(ns emulator-chip8.opcode-spec
  (:require [speclj.core :refer :all]
            [emulator-chip8.opcode :refer :all]))

(describe "1NNN"
          (it "Store number NN in register VX."
              (should= 234
                       (:PC (opcode-1NNN {:PC 0} {:NNN 234})

                            ))
              ))
