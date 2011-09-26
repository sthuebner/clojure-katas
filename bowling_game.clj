(ns bowling-game
  (:use midje.sweet))

(defn parse-roll [roll]
  (condp re-matches (str roll)
    #"\d" (- (int roll) 48)
    #"X" 10
    #"/" :spare
    0))

(facts "about parsing"
  (parse-roll \-) => 0
  (parse-roll \1) => 1
  (parse-roll \9) => 9
  (parse-roll \X) => 10
  (parse-roll \/) => :spare)


(defn next-frame [rolls]
  (rest (re-matches #"^(X|..)(.*)" rolls)))

(facts "about next-frame"
  (next-frame "-------------------") => ["--" "-----------------"]
  (next-frame "1------------------") => ["1-" "-----------------"]
  (next-frame "-1-----------------") => ["-1" "-----------------"]
  (next-frame "5/-----------------") => ["5/" "-----------------"]
  (next-frame "X-----------------")  => ["X"  "-----------------"]
  (next-frame "--") => ["--" ""]
  (next-frame "") => []
  )

(defn spare? [frame]
  (not (nil? (re-matches #"./" frame))))

(facts "about spares"
  (spare? "53") => false
  (spare? "5/") => true)

(defn strike? [frame]
  (not (nil? (re-matches #"X" frame))))

(facts "about strikes"
  (strike? "54") => false
  (strike? "X") => true
  (strike? "XX") => false)

(defn score-game [rolls]
  (loop [[frame left-rolls] (next-frame rolls)
	 score 0]
    (cond
     (not (seq frame)) score
     (spare? frame) (recur (next-frame left-rolls)
			   (+ score 10
			      (parse-roll (first left-rolls))))
     (strike? frame) (recur (next-frame left-rolls)
			    (+ score 10
			       (parse-roll (first left-rolls))
			       (parse-roll (second left-rolls))))
     :else (recur (next-frame left-rolls)
		  (+ score
		     (->> frame (map parse-roll) (apply +)))))))

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
  (future-fact "scoring a perfect game"
	       (score-game "XXXXXXXXXXXX")          => 300)
  (future-fact "scoring a strike and a spare"
	       (score-game "X5/5----------------")  => 40)
  )
