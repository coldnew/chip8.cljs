;; NOTE:
;; This app dosen't us om or reagent since I want to make the tutorial more simple.
;; That's why we still use dom here.

(ns chip8.core
  (:require [chip8.screen :as screen]
            [chip8.rom :as rom]
            [goog.dom :as dom]
            [goog.events :as events]
            [goog.net.EventType :as event-type]
            ))

;; The full application state
(def app-state (atom
                {:screen {:id "canvas"
                          :rows 32
                          :columns 64
                          :scale 10
                          :data (vec (repeat 32
                                             (vec (repeat 64 1))))
                          }
                 :rom {:id "rom_selector"
                       :name ""}
                 :cpu {}
                 }))

;; # Load rom event
;;
;; This function define the chip8 cpu action when user
;; select rom to play. It will also passed the rom data
;; to cpu in js/Unit8Array format.
(defn load-rom
  [name states]
  (let [req (goog.net.XhrIo.)]
    (.setResponseType req "arraybuffer")
    (events/listen req event-type/SUCCESS
                   (fn [n]
                     ;;(cpu/load-rom states (js/Uint8Array. (.getResponse req)))

                     ;; log data
                     ;; (.log js/console  ">>>> "  (js/Uint8Array. (.getResponse req)))
                     ))

    (.send req (str "roms/" name) "GET")))

(defn main []

  ;; Initial Rom Selector
  (rom/refresh-selector (:rom @app-state))

  ;; Initial screen canvas
  (screen/initial (:screen @app-state))

  ;; Initial cpu state

  ;; Track when user select another rom
  (.addEventListener
   (dom/getElement (:id (:rom @app-state))) "change"
   (fn [event]
     (let [rom-name (.-target.value event)]
       ;; We only trigger the event when the
       ;; rom is member in rom/roms
       (when (some #{rom-name} rom/roms)
         ;; Display rom name for debug
         (.log js/console "Select rom: " rom-name)

         (screen/render @app-state)

         ;; update rom-name in app-state
         (swap! app-state assoc-in [:rom] {:name rom-name})


         (load-rom rom-name @app-state)

         ;; Blur rom-selector
         ;; FIXME:
         ;;(.blur (dom/getElement (:id (:rom @app-state))))

         ;; Make focus on canvas
         (.focus (dom/getElement (:id (:screen @app-state))))
         )
       )
     ))

  )