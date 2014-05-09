(ns game-of-life.core
  (:require
   [quil.core :as q]
   [game-of-life.gfx :as gfx]
   [game-of-life.gol :as gol]))




(defn simulation
  "Returns a lazy-seq of future states for a given rule-fn and state"
  [rule-fn state]
  (let [new-state (rule-fn state)]
    (cons new-state (lazy-seq (simulation rule-fn new-state)))))

(defn -main
  []
  (let [
        sim (simulation gol/step gol/game-state)
        ;; time slices are units of time in our simulation
        ;; every time a new slice is pulled out, a new scene is drawn
        time-slices (atom sim)]
    (gfx/sketch time-slices)))


