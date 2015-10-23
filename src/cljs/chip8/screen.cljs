(ns chip8.screen
  (:require [goog.dom :as dom]))

(defn- get-screen
  [state]
  (:screen @state))

(defn- get-screen-elm
  [state key]
  (key (get-screen state)))

(defn get-screen-canvas
  [state]
  (dom/getElement (get-screen-elm state :id)))

(defn- get-context
  ([canvas]
   (get-context canvas "2d"))
  ([canvas type]
   (.getContext canvas type)))

(defn- resize-canvas
  "Get canvas element by id and resize canvas."
  [state]
  (let [canvas  (get-screen-canvas state)
        columns (get-screen-elm state :columns)
        rows    (get-screen-elm state :rows)
        scale   (get-screen-elm state :scale)]
    (.setAttribute canvas "width"   (* columns scale))
    (.setAttribute canvas "height"  (* rows    scale)))
  state)

(defn fill
  "Fill canvas with color."
  [state color]
  (let [canvas (get-screen-canvas state)
        ctx (get-context canvas)]
    (set! (.-fillStyle ctx) color)
    (.fillRect ctx 0 0 (aget canvas "width") (aget canvas "height"))
    (.log js/console
          "canvas width: "  (aget canvas "width")
          "canvas height: " (aget canvas "height")))
  state)

(defn- clear
  "Clear screen canvas by fill of white. (#FFFFFF)"
  [state]
  (fill state "#FFFFFF"))

(defn render
  "Render the canvas according to data."
  [state]
  (let [canvas  (get-screen-canvas state)
        ctx     (get-context canvas)
        columns (get-screen-elm state :columns)
        rows    (get-screen-elm state :rows)
        scale   (get-screen-elm state :scale)]
    ;; clear old canvas first
    (clear state)
    ;; loop through screen, if the element is not zero, draw
    ;; on canvas with black.
    (set! (.-fillStyle ctx) "#000000")
    (dotimes [x columns]
      (dotimes [y rows]
        (if-not (zero? (get-in (get-screen-elm state :data) [y x]))
          (.fillRect ctx (* x scale) (* y scale) scale scale)))))
  state)

(defn initial
  [state]
 ;; (.log js/console "---> " (str state))
  (-> state
      ;; Initial Canvas and resize
      resize-canvas
      ;; Render data to canvas
      (render)))
