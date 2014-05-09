(ns game-of-life.gfx
  (:require [quil.core :as q]))

(def grid-squares 40)
(def grid-size-px 400)
(def spacing (/ grid-size-px grid-squares))


(defn setup
  "Quil Canvas Extranea"
  []
  (q/smooth) ;; Enable AA
  (q/frame-rate 5)
  (q/background 255)
  (q/stroke-weight 3))

(defn clear-canvas
  []
  (q/fill 255)
  (q/rect 0 0 grid-size-px grid-size-px))

(defn draw-cell
  "takes a vector of [x y] and fills that canvas coordinate with a solid colour"
  [coords]
  (let [x (* (- (first coords) 0) spacing)
        y (* (- (second coords) 0) spacing)]
    (q/fill 10 10 200)
    (q/rect x y spacing spacing)))

(defn draw-grid
  "draws a n*n grid"
  [n]
  (let [draw-row (fn [n]
                   (q/line 0 (* spacing n) grid-size-px (* spacing n)))
        draw-col (fn [n]
                   (q/line (* spacing n) 0 (* spacing n) grid-size-px))]
    (loop [c 1]
      (when (<= c n)
        (draw-row c)
        (draw-col c)
        (recur (inc c))))))

(defn draw-state
  "draws living cells (those with a value of 1)"
  [states]
  (doseq [state states]  
    (draw-cell state)))

(defn sketch

  [scenes]
  ;;Define a new quil sketch
    (q/defsketch grid                 
      :title "The Game of Life, by Michael Detmold"  
      :setup setup
      :draw (fn draw-fun
              []
              (clear-canvas)
              (draw-grid grid-squares)
              ;;draw the current simulation state
              (draw-state (first @scenes))
              ;;consume "time" to move to the next state
              (swap! scenes (fn [_] (rest @scenes)))
              )
      :size [grid-size-px grid-size-px] ))
