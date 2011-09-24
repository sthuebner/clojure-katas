;;;; ideas:
;;;; - could finger trees help navigating between frames?
;;;; - or was it zippers?

(ns bowling-game
  (:use midje.sweet))

(defn score-frame [frame-rolls]
  (apply + frame-rolls))

(facts "about frame scores"
  (score-frame [0 0]) => 0
  (score-frame [1 1]) => 2
  (score-frame [5 5]) => 10)



(defn as-frames
  [game-rolls]
  (partition 2 game-rolls))

(fact "about framing"
  (as-frames []) => []
  (as-frames (repeat 20 0)) => (repeat 10 [0 0])
  (as-frames [10 0 5 3]) => [ [10 0] [5 3] ])



(defn strike-frame? [frame-rolls]
  (= [10 0] frame-rolls))

(facts "about strikes"
  (strike-frame? [0 0]) => false
  (strike-frame? [5 5]) => false
  (strike-frame? [10 0]) => true)



(defn spare-frame? [frame-rolls]
  (and (= 10 (reduce + frame-rolls))
       (not (strike-frame? frame-rolls))))

(facts "about spares"
  (spare-frame? [0 0]) => false
  (spare-frame? [5 5]) => true
  (spare-frame? [10 0]) => false)



(defn score-it [[current-score score-fn] next-frame]
  (println current-score)
  (vector
   (+ current-score (score-fn next-frame))
   ;; producing the next score-fn
   (cond
    (strike-frame? next-frame) (fn [frame-rolls]
				 (* 2 (score-frame frame-rolls)))
    (spare-frame? next-frame) (fn [frame-rolls]
				(apply + (* 2 (first frame-rolls))
				       (next frame-rolls)))
    :else score-frame)))

(defn score-game [game-rolls]
  ;; idea: don't even carry state but just functions (like continuations)
  (first (reduce score-it [0 score-frame] (take 10 (as-frames game-rolls)))))



(facts "about Bowling games"
  (fact "scoring a gutter game"
    (score-game (repeat 0)) => 0)
  (fact "scoring a game of 1 pin frames"
    (score-game (repeat 1)) => 20)
  (fact "scoring a one-spare game"
    (score-game (concat [5 5 3] (repeat 0))) => 16)
  (fact "scoring a two-spare game"
    (score-game (concat [5 5 3 7 3] (repeat 0))) => 29)
  (fact "scoring a game of 1 strike, an 8 pin frame, and gutters"
    (score-game (concat [10 0 5 3] (repeat 0))) => 26)
  (future-fact "scoring a perfect game"
    (score-game (cycle [10 0])) => 300)
  )
