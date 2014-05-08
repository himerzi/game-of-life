(ns game-of-life.core
  (:require [quil.core :as q])
  (:use overtone.at-at))


(def grid-squares 40)
(def grid-size-px 400)
(def spacing (/ grid-size-px grid-squares))
(def locations (for [x (range 0 grid-squares) y (range 0 grid-squares)] [y x]))
;; [1 0] [1 1] [1 2]
(def game-state #{[20 19] [21 19] [22 19] [22 17] [21 16] [20 17]})

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

(defn neighbours [[x y]]
  "calculates the coordinates for all of the neighbours of a given 2d coordinate [x y], by doing all combinations on each axis of [-1 0 1]"
  (for [dx [-1 0 1]
        dy (if (zero? dx) [-1 1] [-1 0 1])] 
    [(+ dx x) (+ dy y)]))

(defn step [cells]
  " From Christophe Grand http://clj-me.cgrand.net/2011/08/19/conways-game-of-life/
    This here is our rule function. Takes a sequence with the coordinates of all LIVE cells, and moves it forward one state by  returnig a sequence of coordinates of all cells that are alive in the next state.

   The algorithm works by counting how many live neighbours (n) a paritcular cell (loc) has. It does this in a  relativel intelligent way by only looking at where there are live cells. If a neighbour of a live cell also happens to be the neighbour of 1 or 2 other live cells (this is what frequencies is for), then that cell should live."
  
  (set (for [[loc n] (frequencies (mapcat neighbours cells))
             :when (or (= n 3) (and (= n 2) (cells loc)))]
         loc)))

(defn simulation
  "Returns a lazy-seq of future states for a given rule-fn and state"
  [rule-fn state]
  (let [new-state (rule-fn state)]
    ;; This is an infinitely recursive lazy sequence! Notice how we start
    ;; by considing (prepending) to a new-state onto the head of a not-yet extant
    ;; lazy sequence. 
    ;; You'll notice that the lazy sequence is declared with a
    ;; body that will recurse from the present state, passing the current state into
    ;; itself. Lazy sequences such as this are inherently tail-recursive, so they won't
    ;; blow the stack.
    ;; Play
    (cons new-state (lazy-seq (simulation rule-fn new-state)))))

(defn -main
  []
  (let [
        sim (simulation step game-state)
        ;; time slices are units of time in our simulation
        ;; every time a new slice is pulled out, a new scene is drawn
        time-slices (atom sim)]
    
    ;;Define a new quil sketch
    (q/defsketch grid                 
      :title "The Game of Life, by Michael Detmold"  
      :setup setup
      :draw (fn draw-fun
              []
              (clear-canvas)
              (draw-grid grid-squares)
              ;;draw the current simulation state
              (draw-state (first @time-slices))
              ;;consume "time" to move to the next state
              (swap! time-slices (fn [_] (rest @time-slices)))
              )
      :size [grid-size-px grid-size-px] )))


