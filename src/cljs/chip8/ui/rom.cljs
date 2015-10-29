(ns chip8.ui.rom
  (:require [goog.dom :as dom]
            [goog.events :as events]))

(def rom-selector (dom/getElement "rom_selector"))

;; These roms are locate at resource/publis/roms
;; which is collect from the internet
(def roms ["15PUZZLE" "BLINKY" "BLITZ"   "BRIX"     "CONNECT4"
           "GUESS"    "HIDDEN" "IBM"     "INVADERS" "KALEID"
           "MAZE"     "MERLIN" "MISSILE" "PONG"     "PONG2"
           "PUZZLE"   "SYZYGY" "TANK"    "TETRIS"   "TICTAC"
           "UFO"      "VBRIX"  "VERS"    "WIPEOFF"])

(defn- add-rom
  "Add rom to chooser ID"
  [rom]
  (let [option (dom/createElement "option")]
    ;; (.log js/console rom)
    (set! (.-value option) rom)
    (set! (.-innerHTML option) rom)
    (.appendChild rom-selector option)))

(defn update-rom-selector
  []
  (doseq [r roms]
    (add-rom r)))

(defn on-select-event
  "Add Listener when rom is changed"
  [load-rom-fn]
  (.addEventListener
   rom-selector "change"
   (fn [event]
     (let [rom-name (.-target.value event)]
       ;; We only trigger the event when the
       ;; rom is member in rom/roms
       (when (some #{rom-name} roms)
         (load-rom-fn rom-name))))))


(defn blur-rom-selector
  []
  (.blur rom-selector))