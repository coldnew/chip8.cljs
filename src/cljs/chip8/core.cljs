(ns chip8.core
  (:require [chip8.screen :as screen]
            ))

(def app-state (atom
                {:screen {:id "canvas"
                          :rows 32
                          :columns 64
                          :scale 10
                          :data (vec (repeat 32
                                             (vec (repeat 64 1))))
                          }
                 }))

(defn main []
  ;;(screen/set-canvas! (:screen @app-state))
  ;;(screen/fill (:screen @app-state))

  (screen/initial (:screen @app-state))

  )