(defproject emulator-chip8 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :source-paths ["src/clj"]
  :test-paths ["spec"]

  :dependencies [[org.clojure/clojure "1.8.0"]
                 [org.clojure/clojurescript "1.7.228"]
                 [org.clojure/core.async "0.2.374"]
                 [org.clojure/core.match "0.3.0-alpha4"]
                 [ring "1.4.0"]
                 [com.akolov.enlive-reload "0.2.1"]
                 [ring/ring-defaults "0.1.5"]
                 [slester/ring-browser-caching "0.1.1"]
                 [bk/ring-gzip "0.1.1"]
                 [compojure "1.4.0"]
                 [enlive "1.1.6"]
                 [environ "1.0.2"]]

  :plugins [[lein-cljsbuild "1.1.2"]
            [lein-environ "1.0.2"]
            [speclj "3.3.1"]
            [lein-figwheel "0.5.0-6" :exclusions [org.clojure/core.cache]]]

  :cljsbuild {:builds
              {:app {:source-paths ["src/cljs"]
                     :compiler {:output-to     "resources/public/js/app.js"
                                :output-dir    "resources/public/js/out"
                                :source-map    "resources/public/js/out.js.map"
                                :preamble      ["react/react.min.js"]
                                :optimizations :none
                                :pretty-print  true}}}}

  :profiles {:dev {:source-paths ["env/dev"]
                   :test-paths ["test/clj"]

                   :dependencies [[figwheel "0.5.0-6"]
                                  [figwheel-sidecar "0.5.0-6"]
                                  [com.cemerick/piggieback "0.2.1"]
                                  [org.clojure/tools.nrepl "0.2.12"]
                                  [weasel "0.7.0"]
                                  [speclj "3.3.1"]]

                   :repl-options {:init-ns chip8.server
                                  :nrepl-middleware [cemerick.piggieback/wrap-cljs-repl]}

                   :plugins [[lein-figwheel "0.5.0-6"]]

                   :figwheel {:http-server-root "public"
                              :server-port 3449
                              :css-dirs ["resources/public/css"]}

                   :env {:is-dev true}

                   :cljsbuild {:test-commands { "test" ["phantomjs" "env/test/js/unit-test.js" "env/test/unit-test.html"] }
                               :builds {:app {:source-paths ["env/dev" "test/cljs"]}
                                        :test {:source-paths ["src/cljs" "test/cljs"]
                                               :compiler {:output-to     "resources/public/js/app_test.js"
                                                          :output-dir    "resources/public/js/test"
                                                          :source-map    "resources/public/js/test.js.map"
                                                          :optimizations :simple
                                                          :pretty-print  true}}
                                        }}}
             :prod {:cljsbuild {:builds {:app
                                          {:source-paths ["env/prod"]
                                           :compiler {:optimizations :advanced :pretty-print false}}}}}
             }

  :min-lein-version "2.5.1"

  :main chip8.server)
