(ns emulator-chip8.cpu)

(defrecord State [memory stack SP PC V I DT ST])

(defn make-cpu
  "Create a new chip8 cpu state."
  ([& {:keys [memory stack SP PC V I DT ST]
       :or
       {
        ;; CHIP-8 only has 4096 bytes (4KB) memory (0x0 ~ 0xFFF).
        ;;
        ;; The first 512 bytes, from 0x000 to 0x1FF, are where the
        ;; original interpreter was located, and should not used by programs.
        memory (vec (repeat 4096 0))

        ;; The stack is used to remember the current location
        ;; before a jump is performed. The system has 16 levels
        ;; of stack and in order to remember which level of the
        ;; stack is used.
        stack (vec (repeat 16 0))

        ;; The stack pointer (SP) can be 8-bit, it is used to point
        ;; to the topmost level of the stack.
        SP 0

        ;; Most CHIP-8 programs start at locastion 0x200 (512),
        ;; but some begin at 0x600 (1536).
        ;; The program counter (PC) should be 16-bit, and is used
        ;; to store the currently executing address.
        PC 0x200

        ;; V-registers, CHIP-8 has 15 8-bit general purpose registers
        ;; named V0, V1 ~ VE. The 16th register is used for
        ;; the `carry flag`.
        V (vec (repeat 16 0))

        ;; I-register is used to store memory address
        I 0

        ;; The delay timer is active whenever the delay timer
        ;; register (DT) is non-zero.
        ;; This timer does nothing more than subtract 1 from the
        ;; value of DT at a rate of 60Hz. When DT reaches 0, it deactivates.
        DT 0

        ;; The sound timer is active whenever the sound timer
        ;; register (ST) is non-zero. This timer also decrements
        ;; at a rate of 60Hz, however, as long as ST's value is
        ;; greater than zero, the Chip-8 buzzer will sound. When ST
        ;; reaches zero, the sound timer deactivates.
        ST 0
        }}]

   (State. memory stack SP PC V I DT ST)))

;; Default fontsets, it will be add to memory
(def fontset
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

(defn load-fontset
  "load fonts to chip8's memory."
  [state]
  (let [memory (:memory state)
        c (count memory)]
    (merge state {:memory (vec (take c (into fontset memory)))})))

(defn load-rom
  "Load ROM data to memory."
  [state rom]
  (let [memory (:memory state)
        c (count memory)
        rom1 (vec rom)
        p (count rom1)]
    (merge state
           {:memory
            (vec (take c (into (subvec memory 0 0x200) rom1)))})))

(defn reset-cpu
  "Reset the full cpu state."
  []
  (-> (make-cpu)
      load-fontset)
  ;; TODO: clear screen
  )

(defn step
  "Step through the cpu."
  [state]
  ;; TODO:
  )