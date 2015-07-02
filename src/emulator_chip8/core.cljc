(ns emulator-chip8.core
  (:require #?(:cljs [cljs.nodejs :as nodejs])
            #?(:cljs [goog.crypt :as gcrypt])
            [clojure.string :as str]
            [emulator-chip8.opcode :refer [*opcode-table*]])
  )

;; enable *print-fn* in clojurescript
#?(:cljs (enable-console-print!))

(defn aa []
  (println "this is aa asdasdsadad"))

(def bb {:a aa})

(defn b []
  ((:a bb)))

(defn init
  "Initial chip8 system."
  [])


(defn -main [& args]
  ;; (let [arg1 (nth args 0)]
  ;;   (if arg1
  ;;     (println "TODO:")
  ;;     (println "Error: Please specify filename.")))
  (b)
  (println (str "-->> " @*opcode-table*))
  )

;; setup node.js starter point
#?(:cljs (set! *main-cli-fn* -main))