(ns chip8.ui.core
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

(def request-animation-frame
  (or (.-requestAnimationFrame js/window)
      (.-webkitRequestAnimationFrame js/window)
      (.-mozRequestAnimationFrame js/window)
      (.-oRequestAnimationFrame js/window)
      (.-msRequestAnimationFrame js/window)
      (fn [callback] (js/setTimeout callback (/ 1000 60)))))

(defn run-emulator-loop []
  (request-animation-frame run-emulator-loop)

  (reset! app-state
          (-> @app-state
              (cpu/step)
              (screen/render)
              ))
  )

;;(:memory @app-state)

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

                     (reset! app-state
                             (cpu/load-rom @app-state
                                           (array-seq (js/Uint8Array. (.getResponse req)))))

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
