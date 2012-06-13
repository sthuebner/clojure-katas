;;;; see http://osherove.com/tdd-kata-1/

(ns string-calculator
  (:use [clojure.string :only [split]]
        [midje sweet]))

(defn find-separator [input]
  (if (.startsWith input "//")
    [(.substring input 4)
     (re-pattern (str ",|\\n|" (.substring input 2 3)))]
    [input #",|\n"])
   )

(defn add [input]
  (apply + (map (fn [s]
                  (Integer/parseInt (if (= "" s) "0" s)))
                (apply split (find-separator input)))))


(defn new-random []
  (-> (Math/random) (* 1000) Math/ceil int))

(fact "basics"
  (add "") => 0
  (add "1") => 1
  
  (let [n (new-random)]
    (add (str n)) => n))

(fact "separate numbers by commas"
  (add "1,2") => 3
  (add ",1,2,") => 3)

(fact "support NL as delimiter"
  (add "1\n2,3") => 6
  )


(fact "support more delimiters"
  (add "//;\n1;2,3") => 6
  )
