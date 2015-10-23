(ns chip8.server
  (:require [clojure.java.io :as io]
            [chip8.dev :refer [is-dev? inject-devmode-html browser-repl]]
            [compojure.core :refer [GET defroutes]]
            [compojure.route :refer [resources]]
            [net.cgrand.enlive-html :refer [deftemplate]]
            [net.cgrand.reload :refer [auto-reload]]
            [ring.middleware.reload :as reload]
            [ring.middleware.browser-caching :refer [wrap-browser-caching]]
            [ring.middleware.defaults :refer [wrap-defaults api-defaults]]
            [ring.middleware.gzip :refer [wrap-gzip]]
            [com.akolov.enlive-reload :refer [wrap-enlive-reload]]
            [environ.core :refer [env]]
            [ring.adapter.jetty :refer [run-jetty]])
  (:gen-class))

(deftemplate page (io/resource "public/index.html") []
  [:body] (if is-dev? inject-devmode-html identity))

(defroutes routes
  (resources "/")
  (resources "/react" {:root "react"})
  (GET "/*" req (page)))

(defn- wrap-browser-caching-opts [handler]
  (wrap-browser-caching handler (or (env :browser-caching) {})))

(def http-handler
  (cond-> routes
    true (wrap-defaults api-defaults)
    is-dev? reload/wrap-reload
    is-dev? wrap-enlive-reload
    true wrap-browser-caching-opts
    true wrap-gzip))

(defn run-web-server [& [port]]
  (let [port (Integer. (or port (env :port) 10555))]
    (println (format "Starting web server on port %d." port))
    (run-jetty http-handler {:port port :join? false})))

(defn run-auto-reload [& [port]]
  (println "Starting web server with auto-reload feature.")
  (auto-reload *ns*))

(defn run [& [port]]
  (when is-dev?
    (run-auto-reload))
  (run-web-server port))

(defn -main [& [port]]
  (run port))
