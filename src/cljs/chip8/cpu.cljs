(ns chip8.cpu
  (:require [chip8.screen :as screen]
            [cljs.pprint :refer [cl-format]]
            [cljs.core.match :refer-macros [match]]))

(def fonts
  [0xF0 0x90 0x90 0x90 0xF0 ; 0
   0x20 0x60 0x20 0x20 0x70 ; 1
   0xF0 0x10 0xF0 0x80 0xF0 ; 2
   0xF0 0x10 0xF0 0x10 0xF0 ; 3
   0x90 0x90 0xF0 0x10 0x10 ; 4
   0xF0 0x80 0xF0 0x10 0xF0 ; 5
   0xF0 0x80 0xF0 0x90 0xF0 ; 6
   0xF0 0x10 0x20 0x40 0x40 ; 7
   0xF0 0x90 0xF0 0x90 0xF0 ; 8
   0xF0 0x90 0xF0 0x10 0xF0 ; 9
   0xF0 0x90 0xF0 0x90 0x90 ; A
   0xE0 0x90 0xE0 0x90 0xE0 ; B
   0xF0 0x80 0x80 0x80 0xF0 ; C
   0xE0 0x90 0x90 0x90 0xE0 ; D
   0xF0 0x80 0xF0 0x80 0xF0 ; E
   0xF0 0x80 0xF0 0x80 0x80 ; F
   ])

(defn- make-memory
  "Create CHIP8 memory, which is 4096 bytes (4kb)."
  []
  (let [size 4096]
    (vec
     (take size
           (into fonts
                 (vec (repeat size 0)))))))

(defn- assoc-in-range
  "Update arr with val from start."
  ([arr val] (assoc-in-range arr val 0))
  ([arr val start-or-range]
   (if (sequential? start-or-range)
     (reduce #(assoc-in %1 [%2] (nth val %2)) arr start-or-range)
     (let [bound (range (count val))
           start start-or-range]
       (reduce #(assoc-in %1 [(+ %2 start)] (nth val %2)) arr bound)))))

(defn- get-in-range
  "Get arr from start to end."
  ([arr start end] (get-in-range arr (range start end)))
  ([arr rge]
   (reduce #(conj %1 (get-in arr [%2])) [] rge)))

(defn ->bcd
  "Convert val to BCD array.
  ex: (->bcd 123) => [1 2 3]."
  [val]
  [(int (/ val 100))
   (int (/ (mod val 100) 10))
   (int (mod val 10))])

(defn VxVy [V X Y]
  [(nth V X) (nth V Y)])

(defn read-register
  [state key]
  (key state))

(defn write-pc
  [{:keys [pc] :as state} val]
  (assoc-in state [:pc] val))

(defn increase-pc
  [{:keys [pc] :as state} val]
  (write-pc state (+ pc val)))

(defn write-sp
  [state sp]
  (assoc-in state [:sp] sp))

(defn write-i
  [state i]
  (assoc-in state [:i] i))

(defn write-delay-timer
  [state dt]
  (assoc-in state [:delay-timer] dt))

(defn write-screen
  [state screen]
  (assoc-in state [:screen] screen))

(defn write-sound-timer
  [state st]
  (assoc-in state [:sound-timer] st))

;; FIXME: remove
(defn write-register
  [state key val]
  (assoc-in state [key] val))

(defn write-stack
  ([{:keys [stack] :as state} idx val]
   (write-stack state (assoc stack idx val)))
  ([state stack]
   (assoc-in state [:stack] stack)))

(defn write-memory
  ([{:keys [memory] :as state} arr start]
   (write-memory state (assoc-in-range memory arr start)))
  ([state memory]
   (assoc-in state [:memory] memory)))

(defn write-v
  ([{:keys [v] :as state} X val]
   (write-v state (assoc v X val)))
  ([{:keys [v] :as state} val]
   (assoc-in state [:v] (assoc-in-range v val))))


(def ^{:private true}
  default-state
  {;; CHIP-8 only has 4096 bytes (4KB) memory (0x0 ~ 0xFFF).
   ;;
   ;; The first 512 bytes, from 0x000 to 0x1FF, are where the
   ;; original interpreter was located, and should not used by programs.
   :memory (make-memory)

   ;; The stack is used to remember the current location
   ;; before a jump is performed. The system has 16 levels
   ;; of stack and in order to remember which level of the
   ;; stack is used.
   :stack  (vec (repeat 16 0))

   ;; The stack pointer (SP) can be 8-bit, it is used to point
   ;; to the topmost level of the stack.
   :sp 0

   ;; Most CHIP-8 programs start at locastion 0x200 (512),
   ;; but some begin at 0x600 (1536).
   ;; The program counter (PC) should be 16-bit, and is used
   ;; to store the currently executing address.
   :pc 0x200

   ;; V-registers, CHIP-8 has 15 8-bit general purpose registers
   ;; named V0, V1 ~ VE. The 16th register is used for
   ;; the `VF flag`.
   :v (vec (repeat 16 0))

   ;; I-register is used to store memory address
   :i 0

   ;; The delay timer is active whenever the delay timer
   ;; register (DT) is non-zero.
   ;; This timer does nothing more than subtract 1 from the
   ;; value of DT at a rate of 60Hz. When DT reaches 0, it deactivates.
   :delay-timer 0

   ;; The sound timer is active whenever the sound timer
   ;; register (ST) is non-zero. This timer also decrements
   ;; at a rate of 60Hz, however, as long as ST's value is
   ;; greater than zero, the Chip-8 buzzer will sound. When ST
   ;; reaches zero, the sound timer deactivates.
   :sound-timer 0

   :screen (screen/make-screen)

   ;; Extra state to let the UI know if machine is stop or error message.
   :STOP 0
   :message ""
   :PAUSE 0
   })

;;;; CPU States

(defn make-cpu []
  default-state)

(defn load-rom
  "Load rom to memory. The program will be loaded start at 0x200."
  [state rom]
  (-> state
      (write-memory (vec rom) 0x200)))

;; FIXME:
(defn opcode-00E0
  "Clear the screen."
  [{:keys [pc] :as state}]
  (-> state
      (write-screen (screen/make-screen))
      (increase-pc 2)))

(defn opcode-00EE
  "Return from a subroutine."
  [{:keys [sp stack] :as state}]
  (-> state
      (write-sp (dec sp))
      (write-pc (+ 2 (nth stack (dec sp))))))

(defn opcode-1NNN
  "Jump to address NNN."
  [state NNN]
  ;;(.log js/console (str "NNN: " NNN))
  (-> state
      (write-pc NNN)))

(defn opcode-2NNN
  "Call subroutine at NNN.
  The interpreter increments the stack pointer, then puts the current PC on the
  top of the stack. The PC is then set to NNN."
  [{:keys [pc sp stack] :as state} NNN]
  (-> state
      (write-stack sp pc)
      (write-sp (inc pc))
      (write-pc NNN)))

(defn opcode-3XNN
  "Skip next instruction if VX = NN.
  The interpreter compares register Vx to kk, and if they are equal, increments
  the program counter by 2."
  [{:keys [pc v] :as state} X NN]
  (let [Vx (nth v X)]
    (-> state
        (increase-pc (if (= Vx NN) 4 2)))))

(defn opcode-4XNN
  "Skip next instruction if VX != NN.
  The interpreter compares register Vx to kk, and if they are not equal,
  increments the program counter by 2."
  [{:keys [pc v] :as state} X NN]
  (let [Vx (nth v X)]
    (-> state
        (increase-pc (if-not (= Vx NN) 4 2)))))

(defn opcode-5XY0
  "Skip next instruction if Vx = Vy.
  The interpreter compares register Vx to register Vy, and if they are equal,
  increments the program counter by 2."
  [{:keys [pc v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)]
    (-> state
        (increase-pc (if (= Vx Vy) 4 2)))))

(defn opcode-6XNN
  "Set Vx to NN.
  The interpreter puts the value kk into register Vx."
  [{:keys [pc] :as state} X NN]
  (-> state
      (write-v X NN)
      (increase-pc 2)))

(defn opcode-7XNN
  "Set Vx = Vx + NN.
  Adds the value kk to the value of register Vx, then stores the result in Vx."
  [{:keys [pc v] :as state} X NN]
  (let [Vx (nth v X)]
    (-> state
        (write-v X (bit-and (+ Vx NN) 0xff))
        (increase-pc 2))))

(defn opcode-8XY0
  "Set Vx = Vy.
  Stores the value of register Vy in register Vx."
  [{:keys [pc v] :as state} X Y]
  (let [Vy (nth v Y)]
    (-> state
        (write-v X Vy)
        (increase-pc 2))))

(defn opcode-8XY1
  "Set Vx = Vx OR Vy.
  Performs a bitwise OR on the values of Vx and Vy, then stores the
  result in Vx. A bitwise OR compares the corrseponding
  bits from two values, and if either bit is 1, then the same bit
  in the result is also 1. Otherwise, it is 0."
  [{:keys [pc v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)]
    (-> state
        (write-v  X (bit-or Vx Vy))
        (increase-pc 2))))

(defn opcode-8XY2
  "Set Vx = Vx AND Vy.
  Performs a bitwise AND on the values of Vx and Vy, then stores
  the result in Vx. A bitwise AND compares the corrseponding
  bits from two values, and if both bits are 1, then the same
  bit in the result is also 1. Otherwise, it is 0."
  [{:keys [pc v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)]
    (-> state
        (write-v  X (bit-and Vx Vy))
        (increase-pc 2))))

(defn opcode-8XY3
  "Set Vx = Vx XOR Vy.
  Performs a bitwise exclusive OR on the values of Vx and Vy, then
  stores the result in Vx. An exclusive OR compares the
  corrseponding bits from two values, and if the bits are not
  both the same, then the corresponding bit in the result is set
  to 1. Otherwise, it is 0."
  [{:keys [pc v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)]
    (-> state
        (write-v  X (bit-xor Vx Vy))
        (increase-pc 2))))

(defn opcode-8XY4
  "Set Vx = Vx + Vy, set VF = carry.
  The values of Vx and Vy are added together. If the result is
  greater than 8 bits (i.e., > 255,) VF is set to 1,
  otherwise 0. Only the lowest 8 bits of the result are kept,
  and stored in Vx."
  [{:keys [pc v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)
        sum (+ Vx Vy)]
    (-> state
        (write-v  X (if (> sum 0xff) (- sum 256) sum))
        (write-v 15 (if (> sum 0xff) 1 0))
        (increase-pc 2))))

(defn opcode-8XY5
  "Set Vx = Vx - Vy, set VF = NOT borrow.
  If Vx > Vy, then VF is set to 1, otherwise 0. Then Vy is
  subtracted from Vx, and the results stored in Vx."
  [{:keys [pc v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)
        sub (- Vx Vy)]
    (-> state
        (write-v  X (if (< sub 0) (+ sub 256) sub))
        (write-v 15 (if (> sub 0) 1 0))
        (increase-pc 2))))

(defn opcode-8XY6
  "Set Vx = Vx SHR 1.
  If the least-significant bit of Vx is 1, then VF is set to 1,
  otherwise 0. Then Vx is divided by 2."
  [{:keys [pc v] :as state} X Y]
  (let [Vx (nth v X)]
    (-> state
        (write-v 15 (bit-and Vx 0x01))
        (write-v  X (bit-shift-right Vx 1))
        (increase-pc 2))))

(defn opcode-8XY7
  "Set Vx = Vy - Vx, set VF = NOT borrow.
  If Vy > Vx, then VF is set to 1, otherwise 0. Then Vx is
  subtracted from Vy, and the results stored in Vx."
  [{:keys [pc v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)]
    (-> state
        (write-v 15 (if (> Vx Vy) 0 1))
        (write-v  X (- Vy Vx))
        (increase-pc 2))))

(defn opcode-8XYE
  "Set Vx = Vx SHL 1."
  [{:keys [pc v] :as state} X Y]
  (let [Vx (nth v X)]
    (-> state
        (write-v 15 (bit-and Vx 0x80))
        (write-v  X (bit-shift-left Vx 1))
        (increase-pc 2))))

(defn opcode-9XY0
  "Skip next instruction if Vx != Vy."
  [{:keys [pc v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)]
    (-> state
        (increase-pc (if-not (= Vx Vy) 4 2)))))

(defn opcode-ANNN
  "Set I = NNN."
  [{:keys [pc] :as state} NNN]
  (-> state
      (write-i NNN)
      (increase-pc 2)))

(defn opcode-BNNN
  "Jump to location NNN + V0"
  [{:keys [v] :as state} NNN]
  (let [V0 (nth v 0)]
    (-> state
        (write-pc (+ V0 NNN)))))

(defn opcode-CXNN
  "Set Vx = random byte AND NN"
  [{:keys [pc v] :as state} X NN]
  (-> state
      (write-v X (bit-and (rand-int 256) NN))
      (increase-pc 2)))


(defn- protect-region
  "Prevent v ovrflow on bound."
  [v bound]
  (cond (> v (dec bound)) (protect-region (- v bound) bound)
        (< v 0)           (protect-region (+ v bound) bound)
        :else v))

(defn- set-pixel
  "Set the pixel on screen according x, y.
  Note that the pixel cooridinate is the same as CHIP-8 original
  implementation."
  [{{:keys [columns rows memory]} :screen :as state} x y & [val]]
  (let [nx (protect-region x columns)
        ny (protect-region y rows)
        v (get-in memory [nx ny])
        val-xor (bit-xor v 1)]
    (-> state
        (assoc-in [:screen :memory]
                  (assoc-in memory [nx ny] (or val val-xor)))
        ((fn [state]
           (if (= v 1)
             (write-v state 0xf 1) state))))))

(defn opcode-DXYN
  "Display n-byte sprite starting at memory location I at (Vx, Vy),
  set VF = collision."
  [{:keys [pc v i memory] :as state} X Y N]
  (let [[Vx Vy] (VxVy v X Y)
        width 8
        height N]

    (-> state
        ;; clear VF before we start
        (write-v 0xf 0)
        ;; calculate uarr, a list contains coordinates which we need to update
        ;; the screen memory, we use loop recur to keep the immutable data
        ((fn [state]
           (let [uarr
                 (for [row (range height) :let [sprite (nth memory (+ i row))]
                       col (range width)  :let [sprite (bit-shift-left sprite col)]
                       :when (> (bit-and sprite 0x80) 0)]
                   [(+ Vx col) (+ Vy row)])]
             (if-not (empty? uarr)
               (loop [ar uarr
                      st state]
                 (if (empty? ar)
                   st
                   (recur (next ar)
                          (set-pixel st (nth (first ar) 0) (nth (first ar) 1)))))
               state))))
        (increase-pc 2))))

(defn opcode-FX07
  "Set Vx = delay timer value."
  [{:keys [pc v delay-timer] :as state} X]
  (-> state
      (write-v X delay-timer)
      (increase-pc 2)))

;; FIXME:
;; (defn opcode-FX0A
;;   " Wait for a key press, store the value of the key in Vx."
;;   [{:keys [v] :as state} X]
;;   (let [Vx (nth v X)]
;;     (-> state
;;         (write-delay-timer Vx)
;;         (write-pc 2))))


(defn opcode-FX15
  "Set delay timer = Vx."
  [{:keys [pc v] :as state} X]
  (let [Vx (nth v X)]
    (-> state
        (write-delay-timer Vx)
        (increase-pc 2))))

(defn opcode-FX18
  "Set sound timer = Vx."
  [{:keys [pc v] :as state} X]
  (let [Vx (nth v X)]
    (-> state
        (write-sound-timer Vx)
        (increase-pc 2))))

(defn opcode-FX29
  "Set I = location of sprite for digit Vx."
  [{:keys [pc v] :as state} X]
  (let [Vx (nth v X)]
    (-> state
        (write-i (* 5 Vx))
        (increase-pc 2))))

(defn opcode-FX33
  "Store BCD representation of Vx in memory locations I, I+1, I+2."
  [{:keys [pc i v memory] :as state} X]
  (let [Vx (nth v X)]
    (-> state
        (write-memory (->bcd Vx) i)
        (increase-pc 2))))

(defn opcode-FX55
  "Store registers V0 through Vx in memory starting at location I."
  [{:keys [pc i v memory] :as state} X]
  (-> state
      (write-memory (get-in-range v 0 (inc X)) i)
      (increase-pc 2)))

(defn opcode-FX65
  "Read registers V0 through Vx from memory starting at location I."
  [{:keys [pc i v memory] :as state} X]
  (-> state
      (write-v (get-in-range memory i (+ i X 1)))
      (increase-pc 2)))

(defn opcode-FX1E
  "Set I = I + Vx."
  [{:keys [pc i v] :as state} X]
  (let [Vx (nth v X)]
    (-> state
        (write-i (+ i Vx))
        (increase-pc 2))))

(defn execute
  [{:keys [memory pc] :as state}]
  ;; (.log js/console "opcode 1: " (nth memory pc))
  ;; (.log js/console "opcode 2: " (nth memory (inc pc)))

  ;;(.log js/console "opcode pc: " pc)

  (let [opcode (+ (bit-shift-left (nth memory pc) 8)
                  (nth memory (inc pc)))
        [w x y z] (take-last 4 (seq (cl-format nil "~:@(~4,'0x~)" opcode)))
        NNN (int (str "0x" x y z))
        NN  (int (str "0x" y z))
        X   (int (str "0x" x))
        Y   (int (str "0x" y))
        N   (int (str "0x" z))]


    ;;(.log js/console (str "=> opcode: " (str "0x" w x y z)))

    (match [ w   x   y   z ]
           ["0" "0" "E" "0"] (opcode-00E0 state)
           ["0" "0" "E" "E"] (opcode-00EE state)
           ["1"  _   _   _ ] (opcode-1NNN state NNN)
           ["2"  _   _   _ ] (opcode-2NNN state NNN)
           ["3"  _   _   _ ] (opcode-3XNN state X NN)
           ["4"  _   _   _ ] (opcode-4XNN state X NN)
           ["5"  _   _   _ ] (opcode-5XY0 state X Y)
           ["6"  _   _   _ ] (opcode-6XNN state X NN)
           ["7"  _   _   _ ] (opcode-7XNN state X NN)
           ["8"  _   _  "0"] (opcode-8XY0 state X Y)
           ["8"  _   _  "1"] (opcode-8XY1 state X Y)
           ["8"  _   _  "2"] (opcode-8XY2 state X Y)
           ["8"  _   _  "3"] (opcode-8XY3 state X Y)
           ["8"  _   _  "4"] (opcode-8XY4 state X Y)
           ["8"  _   _  "5"] (opcode-8XY5 state X Y)
           ["8"  _   _  "6"] (opcode-8XY6 state X Y)
           ["8"  _   _  "7"] (opcode-8XY7 state X Y)
           ["8"  _   _  "E"] (opcode-8XYE state X Y)
           ["9"  _   _  "0"] (opcode-9XY0 state X Y)
           ["A"  _   _   _ ] (opcode-ANNN state NNN)
           ["B"  _   _   _ ] (opcode-BNNN state NNN)
           ["C"  _   _   _ ] (opcode-CXNN state X NN)
           ["D"  _   _   _ ] (opcode-DXYN state X Y N)
           ["F"  _  "0" "7"] (opcode-FX07 state X)
           ["F"  _  "1" "5"] (opcode-FX15 state X)
           ["F"  _  "1" "8"] (opcode-FX18 state X)
           ["F"  _  "2" "9"] (opcode-FX29 state X)
           ["F"  _  "3" "3"] (opcode-FX33 state X)
           ["F"  _  "5" "5"] (opcode-FX55 state X)
           ["F"  _  "6" "5"] (opcode-FX65 state X)
           ["F"  _  "1" "E"] (opcode-FX1E state X)
           ;; If we enter here, there's something wrong :(
           :else (-> state
                     (write-register :message (str "ERROR: no such opcode:" "0x" w x y z))
                     (write-register :STOP 1)))

    ))

(defn update-timers [state]
  (when-not (zero? (:PAUSE state))

    )
  )

(defn step [state]
  (-> state
      (execute))
  )
