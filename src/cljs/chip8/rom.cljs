(ns chip8.rom
  (:require [goog.dom :as dom])
  )

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
  (let [rom-selector (dom/getElement (:id state))
        option (dom/createElement "option")]
    (set! (.-value option) rom)
    (set! (.-innerHTML option) rom)
    (.appendChild rom-selector option)
    ;;(.log js/console rom)
    ))

(defn refresh-selector
  "Add all roms to ID in roms"
  [state]
  (loop [r roms]
    (when (seq r)
      (add-rom state (first r))
      (recur (rest r)))))

(defn add-listener
  "Add Listener when rom is changed"
  [state callback-fn]
  (let [rom-selector (dom/getElement (:id state))]
    (.addEventListener rom-selector "change" callback-fn)))
