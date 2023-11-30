(ns advent-of-code-2023.day-01
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day01.txt"))
(def test-input "")

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 24000))}
  [input]
  24000)

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 45000))}
  [input]
  45000)

(comment
  (solve-a input)

  (solve-b input)
  )
