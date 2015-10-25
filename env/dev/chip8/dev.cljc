(ns chip8.dev
  #?(:cljs (:require-macros [chip8.dev :refer [env]]))
  #?(:clj
     (:require
      [environ.core :as environ]
      [net.cgrand.enlive-html :refer [set-attr prepend append html]]
      [cemerick.piggieback :as piggieback]
      [weasel.repl.websocket :as weasel])
     :cljs
     (:require
      [figwheel.client :as figwheel :include-macros true]
      [cljs.core.async :refer [put!]]
      [weasel.repl :as weasel]))
  #?(:clj (:gen-class)))

;; inject development html to enlive
#?(:clj
   (def inject-devmode-html
     (comp
      (set-attr :class "is-dev")
      (prepend (html [:script {:type "text/javascript" :src "/js/out/goog/base.js"}]))
      (prepend (html [:script {:type "text/javascript" :src "/react/react.js"}]))
      (append  (html [:script {:type "text/javascript"} "goog.require('chip8.main')"])))))

#?(:clj
   (defn browser-repl []
     (piggieback/cljs-repl (weasel/repl-env :ip "127.0.0.1" :port 9001))))

;; macro for detect environment in both clj/cljs
#?(:clj
   (defmacro env [kw]
     (environ/env kw)))

(def is-dev?
  (env :is-dev))

#?(:cljs
   (defn start-development-environment [main-fn]
     ;; enable *print-fn* in clojurescript
     (enable-console-print!)
     ;; enable figwheel support
     (figwheel/watch-and-reload
      :websocket-url "ws://localhost:3449/figwheel-ws"
      :jsload-callback main-fn)
     ;; Connect cljs repl withweasel
     (weasel/connect "ws://localhost:9001" :verbose true :print #{:repl :console})))
