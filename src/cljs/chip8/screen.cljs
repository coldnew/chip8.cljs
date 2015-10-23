(ns chip8.screen
  (:require [goog.dom :as dom]))

(defn get-context
  ([canvas]
   (get-context canvas "2d"))
  ([canvas type]
   (.getContext canvas type)))

(defn- resize-canvas
  "Get canvas element by id and resize canvas."
  [state]
  (let [canvas  (dom/getElement (:id state))
        columns (:columns state)
        rows    (:rows state)
        scale   (:scale state)]
    (.setAttribute canvas "width"   (* columns scale))
    (.setAttribute canvas "height"  (* rows    scale)))
  state)

(defn fill
  "Fill canvas with color."
  [state color]
  (let [canvas (dom/getElement (:id state))
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
  (let [canvas (dom/getElement (:id state))
        ctx (get-context canvas)
        scale   (:scale state)
        rows    (:rows state)
        columns (:columns state)]
    ;; clear old canvas first
    (clear state)
    ;; loop through screen, if the element is not zero, draw
    ;; on canvas with black.
    (set! (.-fillStyle ctx) "#000000")
    (dotimes [x columns]
      (dotimes [y rows]
        (if-not (zero? (get-in (:data state) [y x]))
          (.fillRect ctx (* x scale) (* y scale) scale scale)))))
  state)

(defn initial
  [state]
  (-> state
      ;; Initial Canvas and resize
      resize-canvas
      ;; Render data to canvas
      (render)))
