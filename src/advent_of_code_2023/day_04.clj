(ns advent-of-code-2023.day-04
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day04.txt"))
(def test-input "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53\nCard 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19\nCard 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1\nCard 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83\nCard 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36\nCard 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")

(defn exp [x n]
  (reduce * (repeat n x)))

(defn get-my-numbers
  {:test (fn []
           (is= (get-my-numbers "Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11")
                ["74" "77" "10" "23" "35" "67" "36" "11"]))}
  [card]
  (->> (clojure.string/split card #"\|")
       (second)
       (re-seq #"\d+")))

(defn get-winning-numbers
  [card]
  (->> (clojure.string/split card #"\|")
       (first)
       (re-seq #"\d+")
       (rest)
       (into #{})))

(defn get-num-winning
  [card]
  (let [my-numbers (get-my-numbers card)
        winning-numbers (get-winning-numbers card)]
    (reduce (fn [a v]
              (if (contains? winning-numbers v)
                (inc a)
                a))
            0
            my-numbers)))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 13))}
  [input]
  (reduce (fn [a v]
            (let [num-winning (get-num-winning v)]
              (+ a (if (zero? num-winning) 0 (exp 2 (dec num-winning))))))
          0
          (clojure.string/split-lines input)))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 30))}
  [input]
  (let [lines (clojure.string/split-lines input)]
    (first (reduce (fn [[a extra-cards] i]
                     (let [card (nth lines i)
                           num-winning (get-num-winning card)
                           total-copies (+ 1 (get extra-cards i 0))]
                       [(+ a total-copies)
                        (reduce (fn [extra-cards i]
                                  (update extra-cards i (fn [v]
                                                          (+ (or v 0) total-copies))))
                                extra-cards
                                (range (inc i) (+ i num-winning 1)))]))
                   [0 {}]
                   (range (count lines))))))

(comment
  (solve-a input)
  ;; 24542

  (solve-b input)
  ;; 8736438
  )

