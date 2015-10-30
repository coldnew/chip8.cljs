(ns chip8.ui.core
  ;; (:refer-clojure :exclude [loop])
  (:require [chip8.cpu :as cpu]
            [chip8.ui.screen :as screen]
            [chip8.ui.sound :as sound]
            [chip8.ui.keyboard :as keyboard]
            [chip8.ui.rom :as rom]
            [goog.dom :as dom]
            [goog.events :as events]
            [goog.net.EventType :as event-type]))

;; The full application state
(def app-state (atom (cpu/make-cpu)))

(def emulator-loop (atom nil))

(def request-animation-frame
  (reset! emulator-loop
          (or (.-requestAnimationFrame js/window)
              (.-webkitRequestAnimationFrame js/window)
              (.-mozRequestAnimationFrame js/window)
              (.-oRequestAnimationFrame js/window)
              (.-msRequestAnimationFrame js/window)
              (fn [callback] (js/setTimeout callback (/ 1000 60))))))

(defn stop-emulator-loop []
  (js/cancelAnimationFrame @emulator-loop))

(defn start-emulator-loop []
  (request-animation-frame start-emulator-loop)

  ;; check if we need to stop emulator
  ;;(when ())

      (reset! app-state (-> @app-state
                          (cpu/step)
                          (screen/render)))
  (if (nil? @app-state)
    (stop-emulator-loop))
  )

;; (let [{:keys [memory pc]} @app-state
;;       opcode (bit-or (bit-shift-left (nth memory pc) 8)
;;                      (nth memory (inc pc)))
;;       ]
;;   (.log js/console "opcode 1: " (nth memory pc))
;;   (.log js/console "opcode 2: " (nth memory (inc pc)))
;;   (.log js/console "opcode A: " opcode)
;;   )

;; make native arrays sequable
;; ref: https://groups.google.com/forum/#!topic/clojurescript/bMoFWh7VYGg
(extend-protocol ISeqable
  js/Uint8Array
  (-seq [array] (array-seq array 0)))

;; # Load rom event
;;
;; This function define the chip8 cpu action when user
;; select rom to play. It will also passed the rom data
;; to cpu in js/Unit8Array format.
(defn load-rom
  [name]
  (let [req (goog.net.XhrIo.)]
    (.setResponseType req "arraybuffer")
    (events/listen req event-type/SUCCESS
                   (fn [n]

                     ;; restart a new app-state
                     (reset! app-state
                             (-> (cpu/make-cpu)
                                 (cpu/load-rom (js/Uint8Array. (.getResponse req)))))

;;                     (.log js/console (str "-->App-state: " (:memory @app-state)))
                     ;; start the emulator
;;                     (start-emulator-loop)
;;(cpu/step @app-state)

                     ;; log data
                     ;; (.log js/console  ">>>> "  (js/Uint8Array. (.getResponse req)))
                     ))

    (.send req (str "roms/" name) "GET")))

(defn main []

  ;; Initial Rom Selector
  (rom/update-rom-selector)

  ;; Initial screen
  (screen/initial @app-state)

  ;; Initial keyboard event
  (keyboard/initial)

  ;; Track when user select another rom
  (rom/on-select-event
   (fn [rom-name]
     ;; Display rom name for debug
     (.log js/console "Select rom: " rom-name)

     (load-rom rom-name)

     ;; Blur rom-selector
     ;; FIXME:
     ;;(.blur (dom/getElement (:id (:rom @app-state))))
     (rom/blur-rom-selector)
     ;; Make focus on canvas
     ;;        (.focus (screen/get-screen-canvas app-state))
     (screen/focus-canvas)

     )))
