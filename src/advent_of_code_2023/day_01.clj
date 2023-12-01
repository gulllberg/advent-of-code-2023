(ns advent-of-code-2023.day-01
  (:require [ysera.test :refer [is= is is-not]]
            [ysera.collections :refer [index-of]]))

(def input (slurp "src/advent_of_code_2023/inputs/day01.txt"))
(def test-input "1abc2\npqr3stu8vwx\na1b2c3d4e5f\ntreb7uchet")
(def test-input-2 "two1nine\neightwothree\nabcone2threexyz\nxtwone3four\n4nineeightseven2\nzoneight234\n7pqrstsixteen")

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 142))}
  [input]
  (reduce (fn [a line]
            (let [numbers (->> line
                               (re-seq #"\d")
                               (map read-string))]
              (+ a (* 10 (first numbers)) (last numbers))))
          0
          (clojure.string/split-lines input)))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input-2) 281))}
  [input]
  (reduce (fn [a line]
            (let [numbers (->> line
                                (re-seq #"(?=(\d|one|two|three|four|five|six|seven|eight|nine))")
                                (map (fn [[_ d]]
                                       (if-let [i (index-of [nil "one" "two" "three" "four" "five" "six" "seven" "eight" "nine"] d)]
                                         i
                                         (read-string d)))))]
              (+ a (* 10 (first numbers)) (last numbers))))
          0
          (clojure.string/split-lines input)))

(comment
  (solve-a input)
  ;; 54927

  (solve-b input)
  ;; 54581
  )

