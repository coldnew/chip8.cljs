(ns chip8.ui.keyboard
  (:require [clojure.string :as str]))

(defn- event->keystr [event]
  (.fromCharCode js/String (.-which event)))

(defn- mapping-key [keystr]
  (case (str/upper-case keystr)
    "1" 0x01 "2" 0x02 "3" 0x03 "4" 0x0C
    "Q" 0x04 "W" 0x05 "E" 0x06 "R" 0x0D
    "A" 0x07 "S" 0x08 "D" 0x09 "F" 0x0E
    "Z" 0x0A "X" 0x00 "C" 0x0B "V" 0x0F
    nil)
  )


(defn initial []
  (.addEventListener js/window "keydown"
                     (fn [event]
                       ;; (.log js/console event)
                       ;; prevent firefox steal key
                       (.preventDefault event)
                       (.log js/console
                             (mapping-key (event->keystr event))
                             )

                       ))

  (.addEventListener js/window "keyup"
                     (fn [event]

                       ))

  )