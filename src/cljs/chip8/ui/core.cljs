(ns chip8.ui.core
  ;; (:refer-clojure :exclude [loop])
  (:require [chip8.cpu :as cpu]
            [chip8.ui.screen :as screen]
            [chip8.ui.sound :as sound]
            [chip8.ui.keyboard :as keyboard]
            [chip8.ui.rom :as rom]
            [goog.dom :as dom]
            [goog.events :as events]
            [goog.net.EventType :as event-type]
            [goog.net.XhrIo :as gxhr]))

;; The full application state
(def app-state (atom (cpu/make-cpu)))

(def emulator-loop (atom nil))

(def speed (atom 10))

(defn stop-emulator-loop []
  (js/cancelAnimationFrame @emulator-loop)
  (reset! emulator-loop nil))

(defn start-emulator-loop []
  (reset! emulator-loop (js/requestAnimationFrame start-emulator-loop))

  (reset! app-state (-> @app-state
                        (cpu/write-register :key @keyboard/keycode)
                        (cpu/step @speed)
                        (sound/play)
                        (screen/render)))

  (when-not (zero? (:STOP @app-state))
    (.log js/console "STOP Machine")
    (stop-emulator-loop)))

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
(defn ^:export load-rom [name]
  (when (rom/is-valid? name)
    (let [req (goog.net.XhrIo.)]
      (.setResponseType req "arraybuffer")
      (events/listen req event-type/SUCCESS
                     (fn [n]
                       ;; stop emulator
                       (stop-emulator-loop)

                       ;; reset a new app-state
                       (reset! app-state
                               (-> (cpu/make-cpu)
                                   (cpu/load-rom (js/Uint8Array. (.getResponse req)))))

                       ;; start the emulator
                       (start-emulator-loop)))
      (.send req (str "roms/" name) "GET")
      (.log js/console "Select rom: " name))))

(defn main []

  ;; Initial Rom Selector
  (rom/initial)

  ;; Initial screen
  (screen/initial @app-state)

  ;; Initial keyboard event
  (keyboard/initial))
