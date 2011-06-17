;;;; see http://osherove.com/tdd-kata-1/

(ns kata.string-calculator
  (:use clojure.test
	[clojure.string :only [split]]))

(def int (fn [s] (if (= s "") 0 (Integer/parseInt s))))

(deftest integer-parsing
  (is (= 0 (int "0")))
  (is (= 0 (int ""))))


(def add
  (fn [s]
    (let [plus (fn [d ops]
		 (reduce + (map int (split ops d))))]
    (if-let [[_ d ops] (re-matches #"//(.+?)\n(.*)" s)]
      (plus (re-pattern d) ops)
      (plus #"[,\n]" s)))))




(deftest getting-started
  (is (= 0 (add "")) "should return 0 on empty string")
  (is (= 1 (add "1")) "should return operand, if only one argument"))

(deftest two-arguments
  (are [in out] (= (add in) out)
       "1,2" 3
       "2,3" 5
       "1,-1" 0))

(deftest three-or-more-arguments
  (are [in out] (= (add in) out)
       "1,1,1" 3
       "1,2,3" 6))

(deftest NL-separator
  (are [in out] (= (add in) out)
       "1\n2" 3
       "1\n2,3" 6))

(deftest custom-separator
  (are [in out] (= (add in) out)
       "//;\n1;2" 3
       "//;\n1;2;3" 6
       "//#\n1#2#3" 6))

(run-tests)
