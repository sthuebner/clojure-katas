(ns bowling-game
  (:use midje.sweet))

(defprotocol IntegerParser
  (parse-int [this]))

(extend-protocol IntegerParser
  Character
  (parse-int [this]
    (- (int this) 48))
  String
  (parse-int [this]
    (Integer/parseInt this)))

(facts "about parsing integer values"
  (parse-int \1) => 1
  (parse-int "1") => 1)

(defn parse-game [game]
  (->> (re-seq #"\d/|-|\d|X" game)
       (map (fn [s]
	      (condp re-matches s
		#"-" 0
		#"X" 10
		#"\d/" (let [first-roll (parse-int (first s))]
			 [first-roll (- 10 first-roll)])
		(parse-int s))))
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
  (and (not (strike? frame))
       (= 10 (apply + frame))))

(facts "about spares"
  (spare? [5 3]) => false
  (spare? [5 5]) => true
  (spare? [10]) => false)

(defn next-frame [rolls]
  (cond
   (>= 3 (count rolls)) [rolls []]
   (= 10 (first rolls)) [[(first rolls)] (rest rolls)]
   :else [(take 2 rolls) (drop 2 rolls)]))

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
