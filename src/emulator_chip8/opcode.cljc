(ns emulator-chip8.opcode
  #?(:cljs (:require-macros [emulator-chip8.opcode :refer [defop to-opcode-func]])))

#?(:clj
   (defmacro defop
     [name & body]
     (let [name (symbol (str "opcode-" name))
           args [{'r :registers 'm :memory :as 'cpu}
                 'addr-mode
                 ['n 'n-addr]]]
       `(defn ~name ~args ~@body))))

#?(:clj
   (defmacro to-opcode-func [name]
     (let [n  (symbol (str name))]
       `(~n)
       )))

;; 0NNN
(defop 0NNN
  )

;; 00E0
(defn opcode-00E0
  "Clear the screen. This function will also set draw-flag to 1
  to make canvas function refresh."
  [])

;; 00EE
(defn opcode-00EE
  [])

;; 1NNN
(defn opcode-1NNN
  "Jump to address NNN."
  [])

;; 2NNN
(defn opcode-2NNN
  [])

;; 3XNN
(defn opcode-3XNN
  "Skip next instruction if the value of register VX equals NN"
  [])

;; 4XNN
(defn opcode-4XNN
  "Skip next instruction if the value of register VX is
  not equal to NN."
  [])

;; 5XY0
(defn opcode-5XY0
  "Skip next instruction if the value of register VX is
  equal to the value of register VY."
  [])

;; 6XNN
(defn opcode-6XNN
  "Store number NN in register VX."
  [])

;; 7XNN
(defn opcode-7XNN
  "Add the value NN to register VX."
  [])

;; 8XY0
(defn opcode-8XY0
  "Store the value of register VY in register VX."
  [])

;; 8XY1
(defn opcode-8XY1
  "Set VX to VX OR VY."
  [])

;; 8XY2
(defn opcode-8XY2
  "Set VX to VX AND VY."
  [])

;; 8XY3
(defn opcode-8XY3
  "Set VX to VX XOR VY."
  [])

;; 8XY4
(defn opcode-8XY4
  "Add the value of register VY to register VX.
  Set VF to 1 if a carry occurs
  Set VF to 0 if a carry does not occur"
  [])

;; 8XY5
(defn opcode-8XY5
  "Subtract the value of register VY from register VX.
  Set VF to 0 if a borrow occurs
  Set VF to 1 if a borrow does not occur"
  [])

;; 8XY6
(defn opcode-8XY6
  ""
  [])

;; 8XY7
(defn opcode-8XY7
  ""
  [])

;; 8XYE
(defn opcode-8XYE
  ""
  [])

;; 9XY0
(defn opcode-9XY0
  ""
  [])

;; ANNN
(defn opcode-ANNN
  "Store memory address NNN in register I."
  [])

;; BNNN
(defn opcode-BNNN
  ""
  [])

;; CXNN
(defn opcode-CXNN
  "Set VX to a random number with a mask of NN"
  [])

;; DXYN
(defn opcode-DXYN
  ""
  [])

;; EX9E
(defn opcode-EX9E
  ""
  [])

;; EXA1
(defn opcode-EXA1
  ""
  [])

;; FX07
(defn opcode-FX07
  "Store the current value of the delay timer in register VX."
  [])

;; FX0A
(defn opcode-FX0A
  ""
  [])

;; FX15
(defn opcode-FX15
  "Set the delay timer to the value of register VX."
  [])

;; FX18
(defn opcode-FX18
  "Set the sound timer to the value of register VX."
  [])
