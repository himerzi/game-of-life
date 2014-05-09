(ns game-of-life.gol)

(def game-state #{[20 19] [21 19] [22 19] [22 17] [21 16] [20 17]})


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
