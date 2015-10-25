(ns  ^:figwheel-load chip8.cpu-test
  (:require-macros [cljs.test :refer (is deftest testing)])
  (:require [cljs.test]
            [chip8.cpu :as cpu]
            [chip8.screen :as screen]))

(deftest cpu-test
  (let [cpu (:cpu (cpu/make-vm))
        memory (:memory cpu)]

    (testing "memory"
      ;; fonts should at start of memory
      (is (= cpu/fonts (take (count cpu/fonts) memory)))
      ;; memory size should 4096 (4 kb)
      (is (= 4096 (count memory))))

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

    (testing "load-rom"
      (is (= [1 2 3 4]
             (cpu/get-in-range
              (-> (cpu/make-cpu)
                  (cpu/load-rom [1 2 3 4])
                  :memory)
              0x200 0x204))))

    (testing "opcode: 00E0"
      (let [res  (-> (cpu/make-vm) cpu/opcode-00E0)]
        (is (= 2 (-> res :cpu :pc)))
        (is (= 1 (-> res :cpu :draw-flag)))
        (is (= (screen/make-screen) (-> res :screen)))))


    (testing "opcode: 1NNN"
      (let [addr 123
            res  (-> (cpu/make-vm) (cpu/opcode-1NNN addr))]
        (is (= addr (-> res :cpu :pc)))))

    (testing "opcode: 3XNN"
      (let [cpu (cpu/make-vm)]
        (is (= 4 (-> cpu (cpu/opcode-3XNN 2 0) :cpu :pc)))
        (is (= 2 (-> cpu (cpu/opcode-3XNN 2 1) :cpu :pc)))))

    (testing "opcode: 4XNN"
      (let [cpu (cpu/make-vm)]
        (is (= 4 (-> cpu (cpu/opcode-4XNN 3 1) :cpu :pc)))
        (is (= 2 (-> cpu (cpu/opcode-4XNN 3 0) :cpu :pc)))))


    (testing "opcode: 5XY0"
      (let [cpu (-> (cpu/make-vm) (assoc-in [:cpu :v] [0 1 2 1]))]
        (is (= 4 (-> cpu (cpu/opcode-5XY0 3 1) :cpu :pc)))
        (is (= 2 (-> cpu (cpu/opcode-5XY0 1 0) :cpu :pc)))))


    (testing "opcode: 6XNN"
      (let [cpu (cpu/make-vm)]
        (is (= 52 (-> cpu (cpu/opcode-6XNN 5 52) :cpu :v (nth 5))))
        (is (= 13 (-> cpu (cpu/opcode-6XNN 2 13) :cpu :v (nth 2))))))

    (testing "opcode: 7XNN"
      (let [cpu (-> (cpu/make-vm) (assoc-in [:cpu :v] [0 1 2 1]))]
        (is (= 53 (-> cpu (cpu/opcode-7XNN 1 52) :cpu :v (nth 1))))
        (is (= 15 (-> cpu (cpu/opcode-7XNN 2 13) :cpu :v (nth 2))))))

    ))
