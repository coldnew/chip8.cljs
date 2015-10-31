(ns chip8.core
  (:require [chip8.ui.core :as ui]))

;; The real entry point is chip8.ui.core.main
(defn main []
  (ui/main))
