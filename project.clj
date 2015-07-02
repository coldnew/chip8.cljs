(defproject emulator-chip8 "0.1.0-SNAPSHOT"
  :description "FIXME: write description"
  :url "http://example.com/FIXME"
  :license {:name "Eclipse Public License"
            :url "http://www.eclipse.org/legal/epl-v10.html"}

  :source-paths ["src"]
  :test-paths ["spec"]

  :dependencies [[org.clojure/clojure "1.7.0"]
                 [org.clojure/clojurescript "0.0-3308" :scope "provided"]]

  :profiles {:dev {:dependencies [[speclj "3.3.0"]]}}

  :plugins [[lein-cljsbuild "1.0.6"]
            [speclj "3.3.0"]]

  :min-lein-version "2.5.1"

  :cljsbuild {:builds
              [{
                :source-paths ["src"]
                :compiler {:output-to "target/emulator-chip8.js"
                           :output-dir "target"
                           :source-map "target/emulator-chip8.js.map"
                           :target :nodejs
                           :optimizations :none
                           :pretty-print true}}]}

  :aot [emulator-chip8.core]
  :main emulator-chip8.core)
