(ns emulator-chip8.core
  (:require #?(:cljs [cljs.nodejs :as nodejs])
            #?(:cljs [goog.crypt :as gcrypt])
            [clojure.string :as str]))

;; enable *print-fn* in clojurescript
#?(:cljs (enable-console-print!))

(defn -main [& args]
  (let [arg1 (nth args 0)]
    (if arg1
      (println "TODO:")
      (println "Error: Please specify filename."))))

;; setup node.js starter point
#?(:cljs (set! *main-cli-fn* -main))