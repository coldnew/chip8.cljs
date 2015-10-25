(ns chip8.screen
  (:require [goog.dom :as dom]))

(defn- resize-canvas
  "Get canvas element by id and resize canvas."
  [state]
  (let [screen  (:screen state)
        canvas  (dom/getElement (:id screen))
        columns (:columns screen)
        rows    (:rows screen)
        scale   (:scale screen)]
    (.setAttribute canvas "width"   (* columns scale))
    (.setAttribute canvas "height"  (* rows    scale)))
  state)

(defn fill
  "Fill canvas with color."
  [state color]
  (let [screen (:screen state)
        canvas (dom/getElement (:id screen))
        ctx    (.getContext canvas "2d")]
    (set! (.-fillStyle ctx) color)
    (.fillRect ctx 0 0 (aget canvas "width") (aget canvas "height")))
  state)

(defn- clear
  "Clear screen canvas by fill of white. (#FFFFFF)"
  [state]
  (fill state "#FFFFFF"))

(defn set-pixel
  "Set the pixel on screen according x, y.
Note that the pixel cooridinate is the same as CHIP-8 original
implementation."
  [state x y]
  (.log js/console (str "X:" x " Y:" y))

  (let [screen (:screen state)
        val (get-in (:memory screen) [x y])
        memory (assoc-in (:memory screen) [x y] (bit-xor val 1))]
    (-> state
        (assoc-in [:screen :memory] memory))))

(defn render
  "Render the canvas according to screen memory."
  [state]
  (let [screen  (:screen state)
        canvas  (dom/getElement (:id screen))
        ctx     (.getContext canvas "2d")
        columns (:columns screen)
        rows    (:rows screen)
        scale   (:scale screen)]
    ;; clear old canvas first
    (clear state)
    ;; loop through screen, if the element is not zero, draw
    ;; on canvas with black.
    (set! (.-fillStyle ctx) "#000000")
    (dotimes [x columns]
      (dotimes [y rows]
        (when-not (zero? (get-in (:memory screen) [x y]))
          (.fillRect ctx (* x scale) (* y scale) scale scale)))))
  state)

;; ## Screen setup
;;
;; The original implementation of the Chip-8 language used a
;; 64x32-pixel monochrome display with this format:
;;
;;        +----------------------+
;;        | (0,0)       (63,0)   |
;;        | (0,31)      (63,31)  |
;;        +----------------------+
;;
(defn make-screen
  "Create the hashmap used by screen"
  []
  (let [rows    32
        columns 64
        scale   10]
    {:id "canvas"
     :rows rows
     :columns columns
     :scale scale
     :memory (vec (repeat columns
                          (vec (repeat rows 0))))
     }))

(defn initial
  [state]
  (-> state
      ;; Initial Canvas and resize
      resize-canvas
      ;; Render data to canvas
      (render)))
