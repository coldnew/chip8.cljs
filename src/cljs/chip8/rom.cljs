(ns chip8.rom
  (:require [goog.dom :as dom]))

(defn- get-rom [state]
  (:rom @state))

(defn- get-rom-val [state key]
  (key (get-rom state)))

(defn get-rom-id [state]
  (get-rom-val state :id))

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
  (let [rom-selector (dom/getElement (get-rom-val state :id))
        option (dom/createElement "option")]
    ;;(.log js/console rom)
    (set! (.-value option) rom)
    (set! (.-innerHTML option) rom)
    (.appendChild rom-selector option))
  state)

(defn refresh-selector
  "Add all roms to ID in roms"
  [state]
  (loop [r roms]
    (when (seq r)
      (add-rom state (first r))
      (recur (rest r))))
  state)


(defn make-rom
  []
  {:id "rom_selector"
   :name ""})