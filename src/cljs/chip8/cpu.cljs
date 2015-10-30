(ns chip8.cpu
  (:require [chip8.screen :as screen]
            [cljs.pprint :refer [cl-format]]
            [cljs.core.match :refer-macros [match]]
            ))

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

(defn- VxVy [V X Y]
  [(nth V X) (nth V Y)])

(defn- read-register
  [state key]
  (key state))



(defn- write-pc
  [state pc]
  (assoc-in state [:pc] pc))

(defn- write-sp
  [state sp]
  (assoc-in state [:sp] sp))

(defn- write-i
  [state i]
  (assoc-in state [:i] i))

(defn- write-dt
  [state dt]
  (assoc-in state [:dt] dt))

(defn- write-st
  [state st]
  (assoc-in state [:st] st))

(defn- write-draw-flag
  [state draw-flag]
  (assoc-in state [:draw-flag] draw-flag))

(defn write-register
  [state key val]
  (assoc-in state [key] val))


(defn- write-stack
  ([{:keys [stack] :as state} idx val]
   (write-stack state (assoc stack idx val)))
  ([state stack]
   (assoc-in state [:stack] stack)))

(defn- write-memory
  ([{:keys [memory] :as state} arr start]
   (write-memory state (assoc-in-range memory arr start)))
  ([state memory]
   (assoc-in state [:memory] memory)))

(defn- write-v
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
   :dt 0

   ;; The sound timer is active whenever the sound timer
   ;; register (ST) is non-zero. This timer also decrements
   ;; at a rate of 60Hz, however, as long as ST's value is
   ;; greater than zero, the Chip-8 buzzer will sound. When ST
   ;; reaches zero, the sound timer deactivates.
   :st 0

   ;; This is special flag for canvas function to know when
   ;; to update the canvas screen. If the flag not zero,
   ;; update the canvas.
   :draw-flag 0

   ;; FIXME:
   :timer {:delay 0
           :sound 0}

   :screen (screen/make-screen)
   })

;;;; CPU States

(defn make-cpu []
  default-state)

(defn make-vm
  []
  (make-cpu)
  )

;;; BLTIZ
;; [18 23 66 76 73 84 90 32 66 121 32 68 97 118 105 100 32 87 73 78 84 69 82 163
;; 65 96 4 97 9 98 14 103 4 208 30 242 30 112 12 48 64 18 33 240 10 0 224 34 217
;; 240 10 0 224 142 112 163 30 107 31 204 31 140 196 220 178 63 1 18 73 220 178
;; 18 57 202 7 122 1 123 254 220 178 122 255 58 0 18 77 126 255 62 0 18 57 107 0
;; 140 112 109 0 110 0 163 27 221 227 63 0 18 193 59 0 18 129 96 5 224 158 18
;; 135 107 1 136 208 120 2 137 224 121 3 163 30 216 145 129 240 96 5 240 21 240
;; 7 48 0 18 139 59 1 18 171 163 30 49 1 216 145 121 1 57 32 18 171 107 0 49 0
;; 124 255 76 0 18 187 163 27 221 227 125 2 61 64 18 185 109 0 126 1 18 101 0
;; 224 119 2 18 45 163 27 221 227 96 20 97 2 98 11 163 32 208 27 242 30 112 8 48
;; 44 18 205 18 215 96 10 97 13 98 5 163 7 208 21 242 30 112 8 48 42 18 225 128
;; 112 112 254 128 6 163 135 240 51 242 101 96 45 241 41 97 13 208 21 112 5 242
;; 41 208 21 0 238 131 130 131 130 251 232 8 136 5 226 190 160 184 32 62 128 128
;; 128 128 248 128 248 252 192 192 249 129 219 203 251 0 250 138 154 153 248 239
;; 42 232 41 41 0 111 104 46 76 143 190 160 184 176 190 0 190 34 62 52 178 216
;; 216 0 195 195 0 216 216 0 195 195 0 216 216 192 192 0 192 192 0 192 192 0 192
;; 192 0 219 219 219 219 0 24 24 0 24 24 0 24 24 0 219 219 219 219 0 24 24 0 24
;; 24 0 24 24 0 24 24 219 219 0 3 3 0 24 24 0 192 192 0 219 219]

(defn load-rom
  "Load rom to memory. The program will be loaded start at 0x200."
  [state rom]
  (.log js/console (str "=> ROM:" (vec rom)))
  (-> state
      (write-memory (vec rom) 0x200)))

;; FIXME:
(defn opcode-00E0
  "Clear the screen. This function will also set draw-flag to 1
  to make canvas function refresh."
  [state]
  (-> state
      (assoc :screen (screen/make-screen))
      (write-draw-flag 1)
      (write-pc 2)))

(defn opcode-00EE
  "Return from a subroutine."
  [{:keys [sp stack] :as state}]
  (-> state
      (write-sp (dec sp))
      (write-pc (+ 2 (nth stack (dec sp))))))

;; FIXME:
(defn opcode-0NNN
  "Return from a subroutine."
  [{:keys [sp stack] :as state} NNN]
  (-> state
      (write-pc 2)))

(defn opcode-1NNN
  "Jump to address NNN."
  [state NNN]
  (-> state
      (write-pc NNN)))

(defn opcode-2NNN
  "Call subroutine at nnn."
  [{:keys [pc sp stack] :as state} NNN]
  (-> state
      (write-stack sp pc)
      (write-sp (inc pc))
      (write-pc NNN)))

(defn opcode-3XNN
  "Skip next instruction if VX = NN."
  [{:keys [v] :as state} X NN]
  (let [Vx (nth v X)]
    (-> state
        (write-pc (if (= Vx NN) 4 2)))))

(defn opcode-4XNN
  "Skip next instruction if VX != NN."
  [{:keys [v] :as state} X NN]
  (let [Vx (nth v X)]
    (-> state
        (write-pc (if-not (= Vx NN) 4 2)))))

(defn opcode-5XY0
  "Skip next instruction if Vx = Vy."
  [{:keys [v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)]
    (-> state
        (write-pc (if (= Vx Vy) 4 2)))))

(defn opcode-6XNN
  "Set Vx to NN."
  [state X NN]
  (-> state
      (write-v X NN)
      (write-pc 2)))

(defn opcode-7XNN
  "Set Vx = Vx + NN."
  [{:keys [v] :as state} X NN]
  (let [Vx (bit-and (+ (nth v X) NN) 0xff)]
    (-> state
        (write-v X Vx)
        (write-pc 2))))

(defn opcode-8XY0
  "Set Vx = Vy."
  [{:keys [v] :as state} X Y]
  (let [Vy (nth v Y)]
    (-> state
        (write-v  X Vy)
        (write-pc 2))))

(defn opcode-8XY1
  "Set Vx = Vx OR Vy."
  [{:keys [v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)]
    (-> state
        (write-v  X (bit-or Vx Vy))
        (write-pc 2))))

(defn opcode-8XY2
  "Set Vx = Vx AND Vy."
  [{:keys [v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)]
    (-> state
        (write-v  X (bit-and Vx Vy))
        (write-pc 2))))

(defn opcode-8XY3
  "Set Vx = Vx XOR Vy."
  [{:keys [v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)]
    (-> state
        (write-v  X (bit-xor Vx Vy))
        (write-pc 2))))

(defn opcode-8XY4
  "Set Vx = Vx + Vy, set VF = carry."
  [{:keys [v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)
        sum (+ Vx Vy)]
    (-> state
        (write-v  X (if (> sum 0xff) (- sum 256) sum))
        (write-v 15 (if (> sum 0xff) 1 0))
        (write-pc 2))))

(defn opcode-8XY5
  "Set Vx = Vx - Vy, set VF = NOT borrow."
  [{:keys [v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)
        sub (- Vx Vy)]
    (-> state
        (write-v  X (if (< sub 0) (+ sub 256) sub))
        (write-v 15 (if (< sub 0) 1 0))
        (write-pc 2))))

(defn opcode-8XY6
  "Set Vx = Vx SHR 1."
  [{:keys [v] :as state} X Y]
  (let [Vx (nth v X)]
    (-> state
        (write-v 15 (bit-and Vx 0x01))
        (write-v  X (bit-shift-right Vx 1))
        (write-pc 2))))

(defn opcode-8XY7
  "Set Vx = Vy - Vx, set VF = NOT borrow."
  [{:keys [v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)]
    (-> state
        (write-v 15 (if (> Vx Vy) 0 1))
        (write-v  X (- Vy Vx))
        (write-pc 2))))

(defn opcode-8XYE
  "Set Vx = Vx SHL 1."
  [{:keys [v] :as state} X Y]
  (let [Vx (nth v X)]
    (-> state
        (write-v 15 (bit-and Vx 0x80))
        (write-v  X (bit-shift-left Vx 1))
        (write-pc 2))))

(defn opcode-9XY0
  "Skip next instruction if Vx != Vy."
  [{:keys [v] :as state} X Y]
  (let [[Vx Vy] (VxVy v X Y)]
    (-> state
        (write-pc (if-not (= Vx Vy) 4 2)))))

(defn opcode-ANNN
  "Set I = NNN."
  [state NNN]
  (-> state
      (write-i NNN)
      (write-pc 2)))

(defn opcode-BNNN
  "Jump to location NNN + V0"
  [{:keys [v] :as state} NNN]
  (let [V0 (nth v 0)]
    (-> state
        (write-pc (+ V0 NNN)))))

(defn opcode-CXNN
  "Set Vx = random byte AND NN"
  [{:keys [v] :as state} X NN]
  (-> state
      (write-v X (bit-and (rand-int 256) NN))
      (write-pc 2)))


(defn- protect-region
  "Prevent v ovrflow on bound."
  [v bound]
  (cond (> v (dec bound)) (protect-region (- v bound) bound)
        (< v 0)           (protect-region (+ v bound) bound)
        :else v))

(defn set-pixel
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


;; FIXME:
(defn opcode-DXYN
  "Display n-byte sprite starting at memory location I at (Vx, Vy),
  set VF = collision."
  [{:keys [v memory i] :as state} X Y N]
  (let [width 8
        height (bit-and N 0x0F)
        [Vx Vy] (VxVy state X Y)]

    (-> state
        ;; clear VF before we start
        (write-v 0xf 0)
        ;; calculate sprite
        ;; FIXME: dirty
        ((fn [state]
           (let [sprite (atom 0)
                 s (atom state)]
             (dotimes [row height]
               (reset! sprite (nth memory (+ i row)))
               (dotimes [col width]
                 (when (> (bit-and @sprite 0x80) 0)
                   (set-pixel @s (+ Vx col) (+ Vy row)))
                 (reset! sprite (bit-and @sprite 0x80))))
             ;; return state
             @s)))
        (write-pc 2))))

(defn opcode-FX07
  "Set Vx = delay timer value."
  [{:keys [v dt] :as state} X]
  (-> state
      (write-v X dt)
      (write-pc 2)))

;; FIXME:
;; (defn opcode-FX0A
;;   " Wait for a key press, store the value of the key in Vx."
;;   [{:keys [v] :as state} X]
;;   (let [Vx (nth v X)]
;;     (-> state
;;         (write-dt Vx)
;;         (write-pc 2))))


(defn opcode-FX15
  "Set delay timer = Vx."
  [{:keys [v] :as state} X]
  (let [Vx (nth v X)]
    (-> state
        (write-dt Vx)
        (write-pc 2))))

(defn opcode-FX18
  "Set sound timer = Vx."
  [{:keys [v] :as state} X]
  (let [Vx (nth v X)]
    (-> state
        (write-st Vx)
        (write-pc 2))))

(defn opcode-FX29
  "Set I = location of sprite for digit Vx."
  [{:keys [v] :as state} X]
  (let [Vx (nth v X)]
    (-> state
        (write-i (* 5 Vx))
        (write-pc 2))))

(defn opcode-FX33
  "Store BCD representation of Vx in memory locations I, I+1, I+2."
  [{:keys [i v memory] :as state} X]
  (let [Vx (nth v X)]
    (-> state
        (write-memory (->bcd Vx) i)
        (write-pc 2))))

(defn opcode-FX55
  "Store registers V0 through Vx in memory starting at location I."
  [{:keys [i v memory] :as state} X]
  (-> state
      (write-memory (get-in-range v 0 (inc X)) i)
      (write-pc 2)))

(defn opcode-FX65
  "Read registers V0 through Vx from memory starting at location I."
  [{:keys [i v memory] :as state} X]
  (-> state
      (write-v (get-in-range memory i (+ i X 1)))
      (write-pc 2)))

(defn opcode-FX1E
  "Set I = I + Vx."
  [{:keys [i v] :as state} X]
  (let [Vx (nth v X)]
    (-> state
        (write-i (+ i Vx))
        (write-pc 2))))


;;(int (str "0x" "1" "e" ))

(bit-or
 (bit-shift-left 18 8)
 26)

(bit-or
 (bit-shift-left 224 8)
 162)

(defn decoding
  [{:keys [memory pc] :as state}]
;;  (.log js/console "opcode 1: " (nth memory pc))
;;  (.log js/console "opcode 2: " (nth memory (inc pc)))

  (let [opcode (bit-or (bit-shift-left (nth memory pc) 8)
                       (nth memory (inc pc)))
        [w x y z] (take-last 4 (seq (cl-format nil "~4,'0d" opcode)))
        NNN (int (str "0x" x y z))
        NN  (int (str "0x" y z))
        X   (int (str "0x" x))
        Y   (int (str "0x" y))
        N   (int (str "0x" z))]

    (.log js/console (str "=> opcode: " (str "0x" w z y z)))
    (match [ w   x   y   z ]
           ["0" "0" "E" "0"] (opcode-00E0 state)
           ["0" "0" "E" "E"] (opcode-00EE state)
           ;;["0"  _   _   _ ] (opcode-0NNN state NNN)
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
           ;;           :else (.log js/console (str "ERROR: no such opcode:" "0x" w x y z))
           :else (throw (js/Error. (str "ERROR: no such opcode:" "0x" w x y z)))
           )
    )
  )

(defn step [state]
  (-> state
      (decoding))
  )


(comment
  (-> (make-cpu)
      (screen/set-pixel 31 31)
      (screen/set-pixel 1 1)
      (screen/set-pixel 0 31)
      (screen/set-pixel 31 0)
      (screen/set-pixel 33 0)
      (screen/set-pixel 63 0)
      (screen/set-pixel 63 31)
      (screen/set-pixel 64 0)
      (screen/render)
      )

  (:pc (-> (make-cpu)
           (assoc :pc 2)))
  )