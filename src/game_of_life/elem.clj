(ns ^{:author "Michael Detmold"
      :doc "An elementary cellular automaton engine (http://mathworld.wolfram.com/ElementaryCellularAutomaton.html)"}
    game-of-life.elem)

;;assume
(def grid-squares 40)

(defn rule-fn
  "takes a decimal number representing the state of the 3 cells, and the (decimal) rule to apply, returns the next state for the center cell"
  [value rule]
  (let [b-rule (format "%08d" (Integer/parseInt (Integer/toString rule 2)))]
    (get b-rule (- (- (count b-rule) 1) value))))

(defn step
  [coords]
  (for [ ]))
