(ns chip8.main
  (:require [chip8.dev :refer [start-development-environment]]
            [chip8.core :as core]
            [chip8.test-runner :as test]))

;; start the development environment, which add figwheel support
(start-development-environment 'core/main)

;; start the application
(core/main)

;; start test-runner
(test/runner)