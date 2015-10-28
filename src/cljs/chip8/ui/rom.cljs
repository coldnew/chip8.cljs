(ns chip8.ui.rom
  (:require [goog.dom :as dom]))

;; These roms are locate at resource/publis/roms
;; which is collect from the internet
(def roms ["15PUZZLE" "BLINKY" "BLITZ"   "BRIX"     "CONNECT4"
           "GUESS"    "HIDDEN" "IBM"     "INVADERS" "KALEID"
           "MAZE"     "MERLIN" "MISSILE" "PONG"     "PONG2"
           "PUZZLE"   "SYZYGY" "TANK"    "TETRIS"   "TICTAC"
           "UFO"      "VBRIX"  "VERS"    "WIPEOFF"])

(defn- add-rom
  "Add rom to chooser ID"
  [state rom]
  (let [rom-selector (dom/getElement "rom_selector")
        option (dom/createElement "option")]
    ;; (.log js/console rom)
    (set! (.-value option) rom)
    (set! (.-innerHTML option) rom)
    (.appendChild rom-selector option))
  state)

(defn update-rom-selector
  [state]
  (doseq [r roms]
    (add-rom state r))
  state)