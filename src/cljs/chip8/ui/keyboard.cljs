(ns chip8.ui.keyboard
  (:require [clojure.string :as str]))

(def keycode (atom 0))

(defn- event->keystr [event]
  (.fromCharCode js/String (.-which event)))

(defn- mapping-key [keystr]
  (case (str/upper-case keystr)
    "1" 0x01 "2" 0x02 "3" 0x03 "4" 0x0C
    "Q" 0x04 "W" 0x05 "E" 0x06 "R" 0x0D
    "A" 0x07 "S" 0x08 "D" 0x09 "F" 0x0E
    "Z" 0x0A "X" 0x00 "C" 0x0B "V" 0x0F
    0))


(defn initial []
  (.addEventListener js/window "keydown"
                     (fn [event]
                       ;; prevent firefox steal keycode
                       (.preventDefault event)
                       (reset! keycode (mapping-key (event->keystr event)))
  ;;                     (.log js/console (str "You enter: " @keycode))
                       ))

  (.addEventListener js/window "keyup"
                     (fn [event]
                       (reset! keycode 0)
                       )))