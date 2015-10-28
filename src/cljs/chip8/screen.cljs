(ns chip8.screen)

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
     :collision 0
     }))

(defn- protect-region
  "Prevent v ovrflow on bound."
  [v bound]
  (cond (> v (dec bound)) (protect-region (- v bound) bound)
        (< v 0)           (protect-region (+ v bound) bound)
        :else v))

(defn set-pixel
  "Set the pixel on screen according x, y.
  Note that the pixel cooridinate is the same as CHIP-8 original
  implementation."
  [{{:keys [columns rows memory]} :screen :as state} x y & [val]]
  (let [nx (protect-region x columns)
        ny (protect-region y rows)
        val-xor (bit-xor (get-in memory [nx ny]) 1)]
    (-> state
        (assoc-in [:screen :memory]
                  (assoc-in memory [nx ny] (or val val-xor))))))
