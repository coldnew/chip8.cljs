(ns chip8.dev
  #?(:cljs (:require-macros [chip8.dev :refer [env]]))
  (:require
   #?(:clj
      [environ.core :as environ]
      ))
  )


;; macro for detect environment in both clj/cljs
#?(:clj
   (defmacro env [kw]
     (environ/env kw)))

;; In production building, we don't allow is-dev variable set to `true'
(if (env :is-dev)
  (let [warning
        (str "Production environment code is being loaded while the dev environment is active. "
             "You likely have compiled class files lying around from an uberjar build. "
             "Remove the target/ directory and try again.")]

  #?(:clj  (throw (Exception. warning))
     :cljs (throw (js/Error. warning)))))

(def is-dev? false)

;; No development html injection
#?(:clj
   (def inject-devmode-html identity))

;; No browser repl
#?(:clj
   (defn browser-repl []
     (throw (Exception. "Browser connected REPL is not available in prod mode"))))