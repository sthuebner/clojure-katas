;;;; see http://osherove.com/tdd-kata-1/

(ns string-calculator
  (:use [clojure.string :only [split]]
        [midje sweet]))

(defn- parse-int [#^String s]
  (Integer/parseInt s))

(defn split-input [s]
  (split s #"[,\n]"))

(defn add
  [s]
  (if (= "" s)
    0
    (let [parse-numbers (partial map parse-int)
          sum (partial reduce +)]
      (->> s
           split-input
           parse-numbers
           sum))))


(fact "Basics"
  (add "") => 0
  (add "1") => 1
  (add "2") => 2
  )

(fact "separate numbers by comma"
   ;; "," as separator
   (add "1,2") => 3
   (add "1,2,3") => 6
   )

(fact "separate numbers also by NL"
  (add "1\n2,3") => 6)


(future-fact "not working yet"
             (add "//;\n1;2") => 3
             )
