(ns emulator-chip8.opcode
  #?(:cljs (:require-macros [emulator-chip8.opcode :refer [defop]]))
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

(def  ^:dynamic *opcode-list* (atom #{}))

#?(:clj
   (defmacro defop
     [opcode args & body]
     (let [pname (symbol (str "opcode-" (name opcode)))]
       `(do
          (defn ~pname ~args ~@body)

          (swap! *opcode-list* conj ~opcode)))))

;; http://stackoverflow.com/questions/24897818/how-to-add-docstring-support-to-defn-like-clojure-macro

(defop :0NNN
  "Execute machine language subroutine at address NNN"
  [state opcode])

(defop :00E0
  "Clear the screen"
  [])

(defop :00EE
  "Return from a subroutine"
  [])

(defop :1NNN
  "Jump to address NNN"
  [state opcode]
  (merge state {:PC (:NNN opcode)}))

(defop :2NNN
  "Execute subroutine starting at address NNN"
  [state opcode]
  )

(defop :3XNN
  "Skip the following instruction if the value of register VX equals NN"
  [state opcode]
  (if (= (nth (:VX state) (:VX opcode)) (:NN opcode))
    ;; skip next
    (println "TODO")
    ))

(defop :4XNN
  "Skip the following instruction if the value of register VX is not equal to
  NN"
  [])

(defop :5XY0
  "Skip the following instruction if the value of register VX is equal to the value of register VY"
  [])

(defop :6XNN
  "Store number NN in register VX."
  [state opcode]
  (merge state {:VX (assoc (:VX state) (:VX opcode))}))

(defop :7XNN
  "Add the value NN to register VX."
  [])

(defop :8XY0
  "Store the value of register VY in register VX"
  [])

(defop :8XY1
  "Set VX to VX OR VY"
  [])

(defop :8XY2
  "Set VX to VX AND VY"
  [])

(defop :8XY3
  "Set VX to VX XOR VY"
  [])

(defop :8XY4
  "Add the value of register VY to register VX.
  Set VF to 1 if a carry occurs
  Set VF to 0 if a carry does not occur."
  [])

(defop :8XY5
  "Subtract the value of register VY from register VX
  Set VF to 0 if a borrow occurs
  Set VF to 1 if a borrow does not occur."
  [])

(defop :8XY6
  "Store the value of register VY shifted right one bit in register VX.
  Set register VF to the least significant bit prior to the shift."
  [])

(defop :8XY7
  "Set register VX to the value of VY minus VX
  Set VF to 00 if a borrow occurs
  Set VF to 01 if a borrow does not occur"
  [])

(defop :8XYE
  "Store the value of register VY shifted left one bit in register VX.
  Set register VF to the most significant bit prior to the shift."
  [])

(defop :9XY0
  "Skip the following instruction if the value of register VX is not equal to
  the value of register VY."
  [])

(defop :ANNN
  "Store memory address NNN in register I"
  [])

(defop :BNNN
  "Jump to address NNN + V0"
  [])

(defop :CXNN
  "Set VX to a random number with a mask of NN"
  [])

(defop :DXYN
  "Draw a sprite at position VX, VY with N bytes of sprite data starting at the
  address stored in I. Set VF to 01 if any set pixels are changed to unset, and
  00 otherwise"
  [])

(defop :EX9E
  "Skip the following instruction if the key corresponding to the hex value
  currently stored in register VX is pressed."
  [])

(defop :EXA1
  "Skip the following instruction if the key corresponding to the hex value
  currently stored in register VX is not pressed."
  [])

(defop :FX07
  "Store the current value of the delay timer in register VX."
  [])

(defop :FX0A
  "Wait for a keypress and store the result in register VX."
  [])

(defop :FX15
  "Set the delay timer to the value of register VX."
  [])

(defop :FX18
  "Set the sound timer to the value of register VX."
  [])

(defop :FX1E
  "Add the value stored in register VX to register I."
  [])

(defop :FX29
  "Set I to the memory address of the sprite data corresponding to the
  hexadecimal digit stored in register VX."
  [])

(defop :FX33
  "Store the binary-coded decimal equivalent of the value stored in register VX
  at addresses I, I + 1, and I + 2."
  [])

(defop :FX55
  "Store the values of registers V0 to VX inclusive in memory starting at
  address I, I is set to I + X + 1 after operation."
  [])

(defop :FX65
  "Fill registers V0 to VX inclusive with the values stored in memory starting
  at address I, I is set to I + X + 1 after operation."
  [])

;;;;;;

;;(deref *opcode-list*)
;;(reset! *opcode-list* #{})

;; (swap! *opcode-list* conj (keyword (name :aaa)))

;; (swap! *opcode-list* conj :as)

(comment

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

  )