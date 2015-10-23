(ns chip8.dev
  (:require [environ.core :refer [env]]
            [net.cgrand.enlive-html :refer [set-attr prepend append html]]
            [cemerick.piggieback :as piggieback]
            [weasel.repl.websocket :as weasel]
            [figwheel-sidecar.auto-builder :as fig-auto]
            [figwheel-sidecar.core :as fig]
            [clojurescript-build.auto :as auto]
            [clojure.java.shell :refer [sh]]))

(def is-dev? (env :is-dev))

(def inject-devmode-html
  (comp
   (set-attr :class "is-dev")
   (prepend (html [:script {:type "text/javascript" :src "/js/out/goog/base.js"}]))
   (prepend (html [:script {:type "text/javascript" :src "/react/react.js"}]))
   (append  (html [:script {:type "text/javascript"} "goog.require('chip8.main')"]))))

(defn browser-repl []
  (piggieback/cljs-repl (weasel/repl-env :ip "127.0.0.1" :port 9001)))
