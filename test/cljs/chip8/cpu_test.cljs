(ns  ^:figwheel-load chip8.cpu-test
  (:require-macros [cljs.test :refer (is deftest testing)])
  (:require [cljs.test]
            [chip8.cpu :as cpu]
            [chip8.screen :as screen]))
(comment
(deftest cpu-test
  (let [cpu (cpu/make-vm)
        memory (:memory cpu)]

    (testing "memory"
      ;; fonts should at start of memory
      (is (= cpu/fonts (take (count cpu/fonts) memory)))
      ;; memory size should 4096 (4 kb)
      (is (= 4096 (count memory))))

    (testing "load-rom"
      (is (= [1 2 3 4]
             (cpu/get-in-range
              (-> (cpu/make-vm)
                  (cpu/load-rom [1 2 3 4])
                  :memory)
              0x200 0x204))))

    (testing "assoc-in-range"
      (is (= [6 7 8 4 5]
             (cpu/assoc-in-range [1 2 3 4 5] [6 7 8] 0)))

      (is (= [6 7 8 4 5]
             (cpu/assoc-in-range [1 2 3 4 5] [6 7 8])))

      (is (= [1 6 7 8 5]
             (cpu/assoc-in-range [1 2 3 4 5] [6 7 8] 1)))

      (is (= [1 2 6 7 8]
             (cpu/assoc-in-range [1 2 3 4 5] [6 7 8] 2))))

    (testing "get-in-range"
      (is (= [0 1]
             (cpu/get-in-range [0 1 2 3 4 5] 0 2)))
      (is (= [1 2 3]
             (cpu/get-in-range [0 1 2 3 4 5] 1 4))))

    (testing "convert to bcd"
      (is (= [1 2 3] (cpu/->bcd 123)))
      (is (= [0 0 3] (cpu/->bcd 3)))
      (is (= [2 5 5] (cpu/->bcd 0xff))))
    ))

(defn make-test-cpu
  []
  (-> (cpu/make-vm)
      (assoc-in [:i] 5)
      (assoc-in [:v] [1 2 3 4 5 5 7 8 9 10 0x13 0x89 12 13 0xfe 0xff])
      (cpu/load-rom [0xff 0xae 5 6 7 8 9 0xff 0xaa])
      ))


(deftest opcode-test
  (let [cpu
        (-> (cpu/make-vm)
            (assoc-in [:i] 5)
            (assoc-in [:v] [1 2 3 4 5 5 7 8 9 10 0x13 0x89 12 13 0xfe 0xff])
            (cpu/load-rom [0xff 0xae 5 6 7 8 9 0xff 0xaa])
            )
        ]

    (testing "opcode: 00E0"
      (let [res  (-> cpu cpu/opcode-00E0)]
        (is (= 2 (-> res :pc)))
        (is (= 1 (-> res :draw-flag)))
        (is (= (screen/make-screen) (-> res :screen)))))

    (testing "opcode: 1NNN"
      (let [addr 123
            res  (-> cpu (cpu/opcode-1NNN addr))]
        (is (= addr (-> res :pc)))))

    (testing "opcode: 3XNN"
      (is (= 4 (-> cpu (cpu/opcode-3XNN 4 5) :pc)))
      (is (= 2 (-> cpu (cpu/opcode-3XNN 4 1) :pc)))
      (is (= 4 (-> cpu (cpu/opcode-3XNN 0xf 0xff) :pc))))

    (testing "opcode: 4XNN"
      (is (= 4 (-> cpu (cpu/opcode-4XNN 4 1) :pc)))
      (is (= 2 (-> cpu (cpu/opcode-4XNN 4 5) :pc))))

    (testing "opcode: 5XY0"
      (is (= 4 (-> cpu (cpu/opcode-5XY0 4 5) :pc)))
      (is (= 2 (-> cpu (cpu/opcode-5XY0 4 1) :pc))))

    (testing "opcode: 6XNN"
      (is (= 52 (-> cpu (cpu/opcode-6XNN 5 52) :v (nth 5))))
      (is (= 13 (-> cpu (cpu/opcode-6XNN 2 13) :v (nth 2))))
      (is (= 2  (-> cpu (cpu/opcode-6XNN 2 3)  :pc))))

    (testing "opcode: 7XNN"
      (is (= 54 (-> cpu (cpu/opcode-7XNN 1 52) :v (nth 1))))
      (is (= 16 (-> cpu (cpu/opcode-7XNN 2 13) :v (nth 2))))
      (is (= 2  (-> cpu (cpu/opcode-7XNN 2 3)  :pc))))

    (testing "opcode: 8XY0"
      (is (= 3 (-> cpu (cpu/opcode-8XY0 1 2)  :v (nth 1))))
      (is (= 5 (-> cpu (cpu/opcode-8XY0 2 5)  :v (nth 2))))
      (is (= 2 (-> cpu (cpu/opcode-8XY0 2 3)  :pc))))

    (testing "opcode: 8XY1"
      (is (= (bit-or 2 3) (-> cpu (cpu/opcode-8XY1 1 2) :v (nth 1))))
      (is (= (bit-or 3 4) (-> cpu (cpu/opcode-8XY1 2 3) :v (nth 2))))
      (is (= 2 (-> cpu (cpu/opcode-8XY1 2 3) :pc))))

    (testing "opcode: 8XY2"
      (is (= (bit-and 2 3) (-> cpu (cpu/opcode-8XY2 1 2) :v (nth 1))))
      (is (= (bit-and 3 4) (-> cpu (cpu/opcode-8XY2 2 3) :v (nth 2))))
      (is (= 2 (-> cpu (cpu/opcode-8XY2 2 3) :pc))))

    (testing "opcode: 8XY3"
      (is (= (bit-xor 2 3) (-> cpu (cpu/opcode-8XY3 1 2) :v (nth 1))))
      (is (= (bit-xor 3 4) (-> cpu (cpu/opcode-8XY3 2 3) :v (nth 2))))
      (is (= 2 (-> cpu (cpu/opcode-8XY3 2 3) :pc))))

    (testing "opcode: 8XY4"
      (is (= 9 (-> cpu (cpu/opcode-8XY4 3 4) :v (nth 3))))
      (is (= 0 (-> cpu (cpu/opcode-8XY4 3 4) :v (nth 0xF))))
      (is (= 5 (-> cpu (cpu/opcode-8XY4 14 15) :v (nth 4))))
      (is (= 1 (-> cpu (cpu/opcode-8XY4 14 15) :v (nth 0xF))))
      (is (= 0 (-> cpu (cpu/opcode-8XY4 0 1)  :v (nth 0xF)))))


    ;; (testing "opcode: DXYN"
    ;;   (let [cpu (-> (make-test-cpu)
    ;;                 (assoc-in [:i] 0)
    ;;                 ((fn [state]
    ;;                    (let [s (atom state)]
    ;;                    (for [i 16 j 16 :let [k (+ i j)]]
    ;;                      (assoc)
    ;;                      )
    ;;                    @s)
    ;;                    )
    ;;                  )
    ;;                 )
    ;;         ])

    ;;   )

    ;; FIXME: rewrite

    (testing "opcode: FX07"
      (is (= 2 (-> cpu (cpu/opcode-FX07 1) :pc)))
      (is (= 0 (-> cpu (cpu/opcode-FX07 0) :v (nth 0))))
      (is (= 5 (-> cpu (assoc-in [:dt] 5)
                   (cpu/opcode-FX07 0)  :v (nth 0)))))

    ;; TODO: FX0A


    (testing "opcode: FX15"
      (is (= 2 (-> cpu (cpu/opcode-FX15 1) :pc)))
      (is (= 1 (-> cpu (cpu/opcode-FX15 0) :dt))))

    (testing "opcode: FX18"
      (is (= 2 (-> cpu (cpu/opcode-FX18 1)  :pc)))
      (is (= 1 (-> cpu (cpu/opcode-FX18 0)  :st))))

    (testing "opcode: FX29"
      (is (= 2 (-> cpu (cpu/opcode-FX29 1)  :pc)))
      (is (= 10 (-> cpu (cpu/opcode-FX29 1) :i)))
      (is (= 5  (-> cpu (cpu/opcode-FX29 0) :i))))

    (testing "opcode: FX33"
      (is (= 2 (-> cpu (cpu/opcode-FX33 1) :pc)))
      (is (= [0 0 3] (take 3 (-> cpu (assoc-in [:i] 0)
                                 (cpu/opcode-FX33 2)
                                 :memory))))

      (is (= [2 5 5] (take 3 (-> cpu (assoc-in [:i] 0)
                                 (cpu/opcode-FX33 15)
                                 :memory)))))

    (testing "opcode: FX55"
      (is (= 16  (-> cpu (cpu/opcode-FX55 1) :v count)))
      (is (= [1 2 3 4 5]
             (take 5 (-> cpu
                         (assoc-in [:i] 0)
                         (cpu/opcode-FX55 5) :memory))))
      (is (= [1 2 3 4 5 5 7 8 9 10 0x13 0x89 12 13 0xfe 0xff]
             (take 16 (-> cpu
                          (assoc-in [:i] 0)
                          (cpu/opcode-FX55 15) :memory)))))

    (testing "opcode: FX65"
      (is (= 16  (-> cpu (cpu/opcode-FX65 1) :v count)))
      (is (= [0xff 0xae]  (take 2 (-> cpu
                                      (assoc-in [:i] 0x200)
                                      (cpu/opcode-FX65 1) :v))))
      (is (= [0xff 0xae 5 6 7 8 9 0xff 0xaa]
             (take 9 (-> cpu (assoc-in [:i] 0x200)
                         (cpu/opcode-FX65 9) :v)))))

    (testing "opcode: FX1E"
      (is (= 8 (-> cpu (cpu/opcode-FX1E 2) :i)))
      (is (= 15 (-> cpu (cpu/opcode-FX1E 9) :i))))
    ))

)