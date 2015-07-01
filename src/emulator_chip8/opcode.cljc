(ns emulator-chip8.opcode
  ;;  #?(:cljs (:require-macros [emulator-chip8.opcode :refer [defop to-opcode-func]]))
  )

;; #?(:clj
;;    (defmacro defop
;;      [name & body]
;;      (let [name (symbol (str "opcode-" name))
;;            args [{'r :registers 'm :memory :as 'cpu}
;;                  'addr-mode
;;                  ['n 'n-addr]]]
;;        `(defn ~name ~args ~@body))))

;; #?(:clj
;;    (defmacro to-opcode-func [name]
;;      (let [n  (symbol (str name))]
;;        `(~n)
;;        )))

(def opcode-table (atom #{}))

(defmacro defop
  [opcode args & body]
  (let [key (keyword (name opcode))
        pname (symbol (str "opcode-" (name opcode)))
        ]
    `(defn ~pname [~@args] ~@body)
    (swap! opcode-table conj key)
    ))

;; http://stackoverflow.com/questions/24897818/how-to-add-docstring-support-to-defn-like-clojure-macro

(defop :0NNN
  "Execute machine language subroutine at address NNN"
  [])

(defop :00E0
  "Clear the screen"
  [])

(defop :00EE
  "Return from a subroutine"
  [])

(defop :1NNN
  "Jump to address NNN"
  [])

(defop :2NNN
  "Execute subroutine starting at address NNN"
  [])

(defop :3XNN
  "Skip the following instruction if the value of register VX equals NN"
  [])

(defop :4XNN
  "Skip the following instruction if the value of register VX is not equal to
  NN"
  [])

(defop :5XY0
  "Skip the following instruction if the value of register VX is equal to the value of register VY"
  [])

;;;;;;;;;;;

(defop :6XNN
  []
  ;;(println "ad")
  )

(defop :8XY3
  []
  ;;(println "ad")
  )


(deref opcode-table)
(reset! opcode-table #{})

(swap! opcode-table conj (keyword (name :aaa)))

(swap! opcode-table conj :as)

(defn make-opcode-match-list
  [opcode]
  (let [code (format "%04X" opcode)
        ZNNN (format "%SNNN" (subs code 0 1))
        ZXNN (format "%SXNN" (subs code 0 1))
        ZXYN (format "%SXYN" (subs code 0 1))
        ZXYZ (format "%SXY%S" (subs code 0 1) (subs code 3 4))
        ZXZZ (format "%SX%S"  (subs code 0 1) (subs code 2 4))]
    (list code ZNNN ZXNN ZXZZ ZXYN ZXYZ)))

(defn make-opcode-sets
  [opcode]
  ;; Create possible opcode set for handler-list
  (->> (make-opcode-match-list opcode)
       (map keyword)
       (set)))

(def handler-list
  {;; Execute machine language subroutine at address NNN
   ;;   :0NNN 'unimplement

   ;; Clear the screen
   ;;   :00E0 'clear-screen

   ;; Return from a subroutine
   :00EE 'return-from-subroutine

   ;; Jump to address NNN
   :1NNN 'jump-to-address

   ;; Execute subroutine starting at address NNN
   :2NNN 'call-subroutine

   ;; Skip the following instruction if the value of register VX equals NN
   :3XNN 'skip-when-VX-equal-NN

   ;; Skip the following instruction if the value of register VX is
   ;; not equal to NN
   :4XNN 'skip-when-VX-not-equal-NN

   ;; Skip the following instruction if the value of register VX is
   ;; equal to the value of register VY
   :5XY0 'skip-when-VX-equal-VY

   ;; Store number NN in register VX
   :6XNN 'store-NN-in-VX

   ;; Add the value NN to register VX
   :7XNN 'add-NN-to-VX

   ;; Store the value of register VY in register VX
   :8XY0 'store-VY-in-VX

   ;; Set VX to VX OR VY
   :8XY1 'set-VX-to-VX-or-VY

   ;; Set VX to VX AND VY
   :8XY2 'set-VX-to-VX-and-VY

   ;; Set VX to VX XOR VY
   :8XY3 'set-VX-to-VX-xor-VY

   ;; Add the value of register VY to register VX
   ;; Set VF to 1 if a carry occurs
   ;; Set VF to 0 if a carry does not occur
   :8XY4 'add-VY-to-VX

   ;; Subtract the value of register VY from register VX
   ;; Set VF to 0 if a borrow occurs
   ;; Set VF to 1 if a borrow does not occur
   :8XY5 'substract-VY-from-VX

   ;; Store the value of register VY shifted right one bit in register VX
   ;; Set register VF to the least significant bit prior to the
   ;; shift
   :8XY6 'unimplement

   ;; Set register VX to the value of VY minus VX
   ;; Set VF to 00 if a borrow occurs
   ;; Set VF to 01 if a borrow does not occur
   :8XY7 'unimplement

   ;; Store the value of register VY shifted left one bit in register VX
   ;; Set register VF to the most significant bit prior to the shift
   :8XYE 'unimplement

   ;; Skip the following instruction if the value of register VX is
   ;; not equal to the value of register VY
   :9XY0 'unimplement

   ;; Store memory address NNN in register I
   :ANNN 'store-NNN-in-I

   ;; Jump to address NNN + V0
   :BNNN 'unimplement

   ;; Set VX to a random number with a mask of NN
   :CXNN 'set-VX-random-mask-NN

   ;; Draw a sprite at position VX, VY with N bytes of sprite data
   ;; starting at the address stored in I
   ;; Set VF to 01 if any set pixels are changed to unset, and 00
   ;; otherwise
   :DXYN 'unimplement

   ;; Skip the following instruction if the key corresponding to
   ;; the hex value currently stored in register VX is pressed
   :EX9E 'unimplement

   ;; Skip the following instruction if the key corresponding to the
   ;; hex value currently stored in register VX is not pressed
   :EXA1 'unimplement

   ;; Store the current value of the delay timer in register VX
   :FX07 'store-delay-timer-to-VX

   ;; Wait for a keypress and store the result in register VX
   :FX0A 'unimplement

   ;; Set the delay timer to the value of register VX
   :FX15 'store-VX-to-delay-timer

   ;; Set the sound timer to the value of register VX
   :FX18 'store-VX-to-sound-timer

   ;; Add the value stored in register VX to register I
   :FX1E 'unimplement

   ;; Set I to the memory address of the sprite data corresponding
   ;; to the hexadecimal digit stored in register VX
   :FX29 'unimplement

   ;; Store the binary-coded decimal equivalent of the value stored
   ;; in register VX at addresses I, I + 1, and I + 2
   :FX33 'unimplement

   ;; Store the values of registers V0 to VX inclusive in memory
   ;; starting at address I
   ;; I is set to I + X + 1 after operation
   :FX55 'unimplement

   ;; Fill registers V0 to VX inclusive with the values stored in
   ;; memory starting at address I
   ;; I is set to I + X + 1 after operation
   :FX65 'unimplement
   })

(defn find-match-handler
  "Search for matching handler in handler list.
  If nothing find, return nil else return keyword."
  [handler opcode]
  (str "opcode-" (name (some (make-opcode-sets opcode) (keys handler))) )
  )

(defn make-handler-args
  "Parse the code to find how many VX, VY, NNN, NN, N
  and create argument lists."
  [opcode]
  (let [code (format "%04X" opcode)]
    {:NNN (subs code 1 4)
     :NN  (subs code 2 4)
     :N   (subs code 3 4)
     :VX  (subs code 1 2)
     :VY  (subs code 2 3)}))

(comment
  (make-opcode-sets 0xf155) ;; FX55
  (make-handler-args 0xf155) ;; FX55

  (find-match-handler handler-list 0xf155)

  (keys handler-list)
  )


(defn find-match-handler
  "Search for matching handler in handler list.
  If nothing find, return nil else return keyword."
  [handler opcode]
  (some (make-opcode-sets opcode) (keys handler)))




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
