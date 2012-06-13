;;;; see http://osherove.com/tdd-kata-1/

(ns string-calculator
  (:use [clojure.string :only [split]]
        [midje sweet]))

(defn add [input]
  (apply + (map #(Integer/parseInt
                  (if (= "" %) "0" %))
                (split input #","))))


(fact "basics"
  (add "") => 0
  (add "1") => 1
  
  (let [n (-> (Math/random) (* 1000) Math/ceil int)
        s (str n)]
    (add s) => n)

  
  )

(fact "separate numbers by commas"
  (add "1,2") => 3
  (add ",1,2,") => 3)