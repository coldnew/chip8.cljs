(ns chip8.ui.screen
  (:require [goog.dom :as dom]))

(defn- resize-canvas
  "Get canvas element by id and resize canvas."
  [{screen :screen :as state}]
  (let [canvas  (dom/getElement (:id screen))
        columns (:columns screen)
        rows    (:rows screen)
        scale   (:scale screen)]
    (.setAttribute canvas "width"   (* columns scale))
    (.setAttribute canvas "height"  (* rows    scale)))
  state)

(defn fill
  "Fill canvas with color."
  [{screen :screen :as state} color]
  (let [canvas (dom/getElement (:id screen))
        ctx    (.getContext canvas "2d")]
    (set! (.-fillStyle ctx) color)
    (.fillRect ctx 0 0 (aget canvas "width") (aget canvas "height")))
  state)

(defn- clear
  "Clear screen canvas by fill of white. (#FFFFFF)"
  [state]
  (fill state "#FFFFFF"))

(defn- protect-region
  "Prevent v ovrflow on bound."
  [v bound]
  (cond (> v (dec bound)) (protect-region (- v bound) bound)
        (< v 0)           (protect-region (+ v bound) bound)
        :else v))

(defn render
  "Render the canvas according to screen memory."
  [{{:keys [columns rows memory scale id]} :screen :as state}]
  (let [canvas  (dom/getElement id)
        ctx     (.getContext canvas "2d")]
    ;; clear old canvas first
    (clear state)
    ;; loop through screen, if the element is not zero, draw
    ;; on canvas with black.
    (set! (.-fillStyle ctx) "#000000")
    (dotimes [x columns]
      (dotimes [y rows]
        (when-not (zero? (get-in memory [x y]))
          (.fillRect ctx (* x scale) (* y scale) scale scale)))))
  state)
