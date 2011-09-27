(ns bowling-game
  (:use midje.sweet))

(defn parse-roll [roll]
  (let [roll (str roll)]
    (condp re-matches roll
      #"\d" (Integer/parseInt roll)
      #"X" 10
      0)))

(facts "about parsing rolls"
  (parse-roll \-) => 0
  (parse-roll \1) => 1
  (parse-roll \9) => 9
  (parse-roll \X) => 10)

(defn parse-game [game]
  (->> (re-seq #"\d/|-|\d|X" game)
       (map (fn [s]
	      (if (re-matches #"\d/" s)
		(let [first-roll (parse-roll (first s))]
		  [first-roll (- 10 first-roll)])
		(parse-roll s))))
       flatten))

(facts "about parsing games"
  (parse-game "1") => [1]
  (parse-game "11") => [1 1]
  (parse-game "X1") => [10 1]
  (parse-game "XX1") => [10 10 1]
  (parse-game "--") => [0 0]
  (parse-game "5/") => [5 5]
  (parse-game "5/5") => [5 5 5])

(defn strike? [frame]
  (= [10] frame))

(facts "about strikes"
  (strike? [5 4]) => false
  (strike? [10]) => true
  (strike? [10 10]) => false)

(defn spare? [frame]
  (and (= 10 (apply + frame))
       (not (strike? frame))))

(facts "about spares"
  (spare? [5 3]) => false
  (spare? [5 5]) => true
  (spare? [10]) => false)


(defn next-frame [rolls]
  (loop [frame []
	 left-rolls rolls]
    ;; FIXME this conditional is ugly
    (cond
     (not (seq left-rolls)) [frame []]
     (= 1 (count left-rolls)) (recur (concat frame left-rolls) [])
     (and (strike? frame) (= 2 (count left-rolls))) (recur (concat frame left-rolls) [])
     (strike? frame) [frame left-rolls]
     (= 2 (count frame)) [frame left-rolls]
     :else (recur (conj frame (first left-rolls))
		  (rest left-rolls)))))

(facts "about next-frame"
  (next-frame [0 0 0 0]) => [[0 0] [0 0]]
  (next-frame []) => [[] []]
  (next-frame [10 10 10]) => [[10 10 10] []]
  (next-frame [10 10 10 10]) => [[10] [10 10 10]]
  (next-frame [10 5 3 0 0]) => [[10] [5 3 0 0]]
  (next-frame [5 5 3 3]) => [[5 5] [3 3]])

(defn score-game [rolls]
  (loop [[frame left-rolls] (next-frame (parse-game rolls))
	 score 0]
    (cond
     (not (seq frame)) score
     (spare? frame) (recur (next-frame left-rolls)
			   (+ score 10 (first left-rolls)))
     (strike? frame) (recur (next-frame left-rolls)
			    (+ score 10 (first left-rolls) (second left-rolls)))
     :else (recur (next-frame left-rolls)
		  (apply + score frame)))))

(facts "about scoring games"
  (score-game "--------------------")  => 0
  (score-game "1-------------------")  => 1
  (score-game "1-1-----------------")  => 2
  (score-game "53------------------")  => 8
  (score-game "9-9-9-9-9-9-9-9-9-9-")  => 90
  (score-game "5/------------------")  => 10
  (score-game "5/5-----------------")  => 20
  (score-game "5/5/5/5/5/5/5/5/5/5/5") => 150
  (score-game "X------------------")   => 10
  (score-game "X5-----------------")   => 20
  (score-game "X53----------------")   => 26
  (score-game "XX----------------")    => 30
  (score-game "XXXXXXXXXXXX")          => 300
  (score-game "X5/5----------------")  => 40)
