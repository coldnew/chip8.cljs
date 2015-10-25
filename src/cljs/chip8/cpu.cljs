(ns chip8.cpu
  (:require [chip8.screen :as screen]
            [goog.dom :as dom])
  )


(defn- make-memory
  "Create CHIP8 memory, which is 4096 bytes (4kb)."
  []
  (let [fonts [0xF0 0x90 0x90 0x90 0xF0 ; 0
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
               ]
        size 4096]
    (vec
     (take size
           (into fonts
                 (vec (repeat size 0)))))))

(defn- assoc-in-range
  "Update arr with val from start."
  ([arr val] (assoc-in-range arr val 0))
  ([arr val start]
  (let [bound (range (count val))]
    (reduce #(assoc-in %1 [(+ %2 start)] (nth val %2)) arr bound))))

(defn- get-in-range
  "Get arr from start to end."
  ([arr start end] (get-in-range arr (range start end)))
  ([arr rge]
   (reduce #(conj %1 (get-in arr [%2])) [] rge)))


;;;; CPU States

(defn make-cpu []
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

   ;; This is special flag for canvas function to know when
   ;; to update the canvas screen. If the flag not zero,
   ;; update the canvas.
   :draw-flag 0
   })

(defn make-vm
  []
  {:cpu (make-cpu)
   :screen (screen/make-screen)
   }
  )

(defn load-rom
  "Load rom to memory. The program will be
  loaded start at 0x200."
  [state rom]
  (.log js/console  "Load ROM --> "  rom)
  (let [mem (:memory state)
        c   (count mem)
        p   (count (vec rom))]
    (assoc state :memory (vec (into
                               (into (subvec mem 0 0x200) (vec rom))
                               (drop (+ 0x200 p) mem))))))

(defn opcode-00E0
  "Clear the screen. This function will also set draw-flag to 1
  to make canvas function refresh."
  [state]
  (-> state
      (assoc :screen (screen/make-screen))
      (assoc-in [:cpu :draw-flag] 1)
      (assoc-in [:cpu :pc] 2)))

(defn opcode-1NNN
  "Jump to address NNN."
  [state NNN]
  (-> state
      (assoc-in [:cpu :pc] NNN)))

(defn opcode-3XNN
  "Skip next instruction if VX = NN."
  [state X NN]
  (let [Vx (nth (:v (:cpu state)) X)
        pc-inc (if (= Vx NN) 4 2)]
    (-> state
        (assoc-in [:cpu :pc] pc-inc))))


(defn opcode-4XNN
  "Skip next instruction if VX != NN."
  [state X NN]
  (let [Vx (nth (:v (:cpu state)) X)
        pc-inc (if-not (= Vx NN) 4 2)]
    (-> state
        (assoc-in [:cpu :pc] pc-inc))))

(defn opcode-5XY0
  "Skip next instruction if Vx = Vy."
  [state X Y]
  (let [Vx (nth (:v (:cpu state)) X)
        Vy (nth (:v (:cpu state)) Y)
        pc-inc (if (= Vx Vy) 4 2)]
    (-> state
        (assoc-in [:cpu :pc] pc-inc))))

(defn opcode-6XNN
  "Set Vx to NN."
  [state X NN]
  (let [V (:v (:cpu state))
        v-new (assoc V X NN)]
    (-> state
        (assoc-in [:cpu :v] v-new)
        (assoc-in [:cpu :pc] 2))))

(defn opcode-7XNN
  "Set Vx = Vx + NN."
  [state X NN]
  (let [V (:v (:cpu state))
        Vx (bit-and (+ (nth V X) NN) 0xff)]
    (-> state
        (assoc-in [:cpu :v] (assoc V X Vx))
        (assoc-in [:cpu :pc] 2))))

(defn opcode-8XY0
  "Set Vx = Vy."
  [state X Y]
  (let [V (:v (:cpu state))
        Vy (nth V Y)]
    (-> state
        (assoc-in [:cpu :v] (assoc V X Vy))
        (assoc-in [:cpu :pc] 2))))

(defn opcode-8XY1
  "Set Vx = Vx OR Vy."
  [state X Y]
  (let [V (:v (:cpu state))
        Vx (nth V X)
        Vy (nth V Y)]
    (-> state
        (assoc-in [:cpu :v] (assoc V X (bit-or Vx Vy)))
        (assoc-in [:cpu :pc] 2))))

(defn opcode-8XY2
  "Set Vx = Vx AND Vy."
  [state X Y]
  (let [V (:v (:cpu state))
        Vx (nth V X)
        Vy (nth V Y)]
    (-> state
        (assoc-in [:cpu :v] (assoc V X (bit-and Vx Vy)))
        (assoc-in [:cpu :pc] 2))))

(defn opcode-8XY3
  "Set Vx = Vx XOR Vy."
  [state X Y]
  (let [V (:v (:cpu state))
        Vx (nth V X)
        Vy (nth V Y)]
    (-> state
        (assoc-in [:cpu :v] (assoc V X (bit-xor Vx Vy)))
        (assoc-in [:cpu :pc] 2))))

(defn opcode-8XY4
  "Set Vx = Vx + Vy, set VF = carry."
  [state X Y]
  (let [V (:v (:cpu state))
        Vx (nth V X)
        Vy (nth V Y)
        sum (+ Vx Vy)
        VF (if (> sum 0xFF) 1 0)]
    (-> state
        (assoc-in [:cpu :v] (assoc V X (bit-and sum 0xFF)))
        (assoc-in [:cpu :v] (assoc V 0xF VF))
        (assoc-in [:cpu :pc] 2))))

(defn opcode-8XY5
  "Set Vx = Vx - Vy, set VF = NOT borrow."
  [state X Y]
  (let [V (:v (:cpu state))
        Vx (nth V X)
        Vy (nth V Y)
        sub (- Vx Vy)
        VF (if (> Vx Vy) 0 1)]
    (-> state
        (assoc-in [:cpu :v] (assoc V X (bit-and sub 0xFF)))
        (assoc-in [:cpu :v] (assoc V 0xF VF))
        (assoc-in [:cpu :pc] 2))))

(defn opcode-8XY6
  "Set Vx = Vx SHR 1."
  [state X]
  (let [V (:v (:cpu state))
        Vx (nth V X)
        shr (bit-shift-right Vx 1)
        VF (bit-and Vx 0x1)]
    (-> state
        (assoc-in [:cpu :v] (assoc V X (bit-and VF 0xFF)))
        (assoc-in [:cpu :v] (assoc V 0xF VF))
        (assoc-in [:cpu :pc] 2))))

(defn opcode-8XY7
  "Set Vx = Vy - Vx, set VF = NOT borrow."
  [state X Y]
  (let [V (:v (:cpu state))
        Vx (nth V X)
        Vy (nth V Y)
        sub (- Vy Vx)
        VF (if (> Vy Vx) 0 1)]
    (-> state
        (assoc-in [:cpu :v] (assoc V Y sub))
        (assoc-in [:cpu :v] (assoc V 0xF VF))
        (assoc-in [:cpu :pc] 2))))

(defn opcode-8XYE
  "Set Vx = Vx SHL 1."
  [state X]
  (let [V (:v (:cpu state))
        Vx (nth V X)
        shl (bit-shift-left Vx 1)
        VF (bit-and Vx 0x80)]
    (-> state
        (assoc-in [:cpu :v] (assoc V X shl))
        (assoc-in [:cpu :v] (assoc V 0xF VF))
        (assoc-in [:cpu :pc] 2))))


(defn opcode-9XY0
  "Skip next instruction if Vx != Vy."
  [state X Y]
  (let [V (:v (:cpu state))
        Vx (nth V X)
        Vy (nth V Y)
        pc-inc (if-not (= Vx Vy) 4 2)]
    (-> state
        (assoc-in [:cpu :pc] pc-inc))))

(defn opcode-ANNN
  "Set I = NNN."
  [state NNN]
  (-> state
      (assoc-in [:cpu :i] NNN)
      (assoc-in [:cpu :pc] 2)))

(defn opcode-BNNN
  "Jump to location nnn + V0"
  [state NNN]
  (let [V (:v (:cpu state))
        V0 (nth V 0)]
    (-> state
        (assoc-in [:cpu :pc] (+ V0 NNN)))))

(defn opcode-CXNN
  "Set Vx = random byte AND NN"
  [state X NN]
  (let [V (:v (:cpu state))
        Vx (bit-and (rand-int 256) NN)]
    (-> state
        (assoc-in [:cpu :v] (assoc V X Vx))
        (assoc-in [:cpu :pc] 2))))


;; (defn opcode-DXYN
;;   "Display n-byte sprite starting at memory location I at (Vx, Vy),
;;   set VF = collision."
;;   [state X Y N]
;;   (let [V (:v (:cpu state))
;;         Vx (bit-and (rand-int 256) NN)
;;         ]
;;     (-> state
;;         (assoc-in [:cpu :v] (assoc V 0xF 0))
;;         ;;
;;         (assoc-in [:cpu :pc] 2))))


(defn opcode-FX55
  "Store registers V0 through Vx in memory starting at location I."
  [state X]
  (let [I (:i (:cpu state))
        V (:v (:cpu state))
        mem (:memory (:cpu state))
        memory (assoc-in-range mem V I)]
    (-> state
        (assoc-in [:cpu :memory] memory)
        (assoc-in [:cpu :pc] 2))))

(defn opcode-FX65
  "Read registers V0 through Vx from memory starting at location I."
  [state X]
  (let [I (:i (:cpu state))
        memory (:memory (:cpu state))
        V (assoc-in-range (:v (:cpu state))
                          (get-in-range memory I (+ I X)))]
    (-> state
        (assoc-in [:cpu :v] V)
        (assoc-in [:cpu :pc] 2))))


(-> (make-vm)
    (assoc-in [:cpu :i] 4)
    (opcode-FX55 3)
    :cpu
    :memory)

(-> (make-vm)
    (assoc-in [:cpu :i] 4)
    (opcode-FX65 3)
    :cpu
    :v)

(->> (make-vm)
     :cpu
     :memory
     (drop 3)
     (take 16))

(defn opcode-FX1E
  "Set I = I + Vx."
  [state X]
  (let [V (:v (:cpu state))
        Vx (nth V X)
        I (:i (:cpu state))]
    (-> state
        (assoc-in [:cpu :i] (+ I Vx))
        (assoc-in [:cpu :pc] 2))))

(comment

  ;; create the cpu state and load memory into it
  (->  (make-cpu)
       (load-rom [0x1 0x2 0x3 0x4]))

  ;; test change draw flag
  (:cpu (assoc-in (make-vm) [:cpu :draw-flag] 1))

  (:v (:cpu (opcode-6XNN (make-vm) 3 10)))

  (defn a []
    (-> (make-cpu)
        (assoc-in [:cpu :v] [0 254 2 3 4 5 6 7 8 9 10 11 12 13 14 15])
        ))

  (-> (a)
      (opcode-8XY4 1 3)
      :cpu
      :v)

  (:memory (:screen (screen/set-pixel (make-vm) 0 1)))

  (-> (make-vm)
      (screen/set-pixel 31 31)
      (screen/set-pixel 1 1)
      (screen/set-pixel 0 31)
      (screen/set-pixel 31 0)
      (screen/set-pixel 33 0)
      (screen/set-pixel 63 0)
      (screen/set-pixel 63 31)
      ;; test value overflow
      (screen/set-pixel 64 64)
      (screen/render))
  )



(defn initial-vm [state]

  ;; Initial screen canvas
  (screen/initial (make-vm))

  (-> (make-vm)
      (screen/set-pixel 31 31)
      (screen/set-pixel 1 1)
      (screen/set-pixel 0 31)
      (screen/set-pixel 31 0)
      (screen/set-pixel 33 0)
      (screen/set-pixel 63 0)
      (screen/set-pixel 63 31)
      (screen/set-pixel 64 0)
      (screen/render))
  )