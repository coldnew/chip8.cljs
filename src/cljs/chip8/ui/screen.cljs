(ns chip8.ui.screen
  (:require [goog.dom :as dom]))


(def canvas (dom/getElement "canvas"))
(def ctx (.getContext canvas "2d"))
(def scale 10)


(defn- resize-canvas
  "Get canvas element by id and resize canvas."
  [{screen :screen :as state}]
  (let [columns (:columns screen)
        rows    (:rows screen)]
    (.setAttribute canvas "width"   (* columns scale))
    (.setAttribute canvas "height"  (* rows    scale)))
  state)

(defn- fill
  "Fill canvas with color."
  [{screen :screen :as state} color]
  (set! (.-fillStyle ctx) color)
  (.fillRect ctx 0 0 (aget canvas "width") (aget canvas "height"))
  state)

(defn- clear
  "Clear screen canvas by fill of white. (#FFFFFF)"
  [state]
  (fill state "#FFFFFF"))

(defn render
  "Render the canvas according to screen memory."
  [{{:keys [columns rows memory]} :screen :as state}]
  ;; clear old canvas first
;;  (.log js/console "===================================")
  (clear state)
  ;; loop through screen, if the element is not zero, draw
  ;; on canvas with black.
  (set! (.-fillStyle ctx) "#000000")
  (dotimes [x columns]
    (dotimes [y rows]
      (when-not (zero? (get-in memory [x y]))
        (.fillRect ctx (* x scale) (* y scale) scale scale))))
  state)

(defn focus-canvas
  []
  (.focus canvas))


(defn initial
  [state]
  (doto state
    ;; Initial Canvas and resize
    (resize-canvas)
    ;; Render data to canvas
    (render)))