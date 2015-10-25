(ns chip8.main
  (:require [chip8.dev :refer [start-development-environment]]
            [chip8.core :as core]))

;; start the development environment, which add figwheel support
(start-development-environment 'core/main)

;; start the application
(core/main)
