(ns chip8.ui.sound)

(def AudioContext
  (or (.-AudioContext js/window)
      (.-webkitAudioContext js/window)))

(def ctx (AudioContext.))
(def osc (.createOscillator ctx))

(defn start-buzzer []
  (doto osc
    (aset "type" "sine")
    (aset "value.frequency" 440)
    (.connect (.-destination ctx))
    (.start)))

(defn stop-buzzer []
  (.stop osc))