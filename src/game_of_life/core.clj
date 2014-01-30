(ns game-of-life.core
  (:require [quil.core :as q])
  (:use overtone.at-at))


(defn game-func
  "function taking a game state to the following game state"
  [state]
  nil)

(def grid-squares 40)
(def grid-size-px 400)
(def spacing (/ grid-size-px grid-squares))
(def locations (for [x (range 0 grid-squares) y (range 0 grid-squares)] [y x]))
;; [1 0] [1 1] [1 2]
(def game-state #{[20 19] [21 19] [22 19] [22 17] [21 16] [20 17]})
(defn setup
  "Setup the UI"
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
  "takes a vector of [x y]"
  [coords]
  (let [x (* (- (first coords) 0) spacing)
        y (* (- (second coords) 0) spacing)]
    (q/fill 10 10 200)
    (q/rect x y spacing spacing)))
(defn draw-grid
  "draw n*n grid"
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
  (for [dx [-1 0 1] dy (if (zero? dx) [-1 1] [-1 0 1])] 
    [(+ dx x) (+ dy y)]))

(defn step [cells]
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

(defn run-game
  []
  (let [
        sim (simulation step game-state)
        time-slices (atom sim)]
    (q/defsketch grid                  ;;Define a new sketch named example
      :title "The Game of Life, by Michael Detmold"  ;;Set the title of the sketch
      :setup setup                      ;;Specify the setup fn
      :draw (fn draw-fun []
              (clear-canvas)
              (draw-grid grid-squares)
              ;; (draw-state (second @time-slices))
              ;;(print (first time-slices))
              (draw-state (first @time-slices))
              (swap! time-slices (fn [_] (rest @time-slices)))
              )
      ;;  :no-loop True
      :size [grid-size-px grid-size-px] )))


