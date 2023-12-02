(ns advent-of-code-2023.day-02
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day02.txt"))
(def test-input "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green\nGame 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue\nGame 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red\nGame 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red\nGame 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green\n")

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 8))}
  [input]
  (reduce (fn [a line]
            (let [game-id (->> line
                               (re-find #"Game (\d+):")
                               (second)
                               (read-string))
                  max-reds (->> line
                                (re-seq #"(\d+) red")
                                (map second)
                                (map read-string)
                                (apply max))
                  max-greens (->> line
                                  (re-seq #"(\d+) green")
                                  (map second)
                                  (map read-string)
                                  (apply max))
                  max-blues (->> line
                                 (re-seq #"(\d+) blue")
                                 (map second)
                                 (map read-string)
                                 (apply max))
                  possible (and (<= max-reds 12) (<= max-greens 13) (<= max-blues 14))]
              (if possible
                (+ a game-id)
                a)))
          0
          (clojure.string/split-lines input)))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 2286))}
  [input]
  (reduce (fn [a line]
            (let [max-reds (->> line
                                (re-seq #"(\d+) red")
                                (map second)
                                (map read-string)
                                (apply max))
                  max-greens (->> line
                                  (re-seq #"(\d+) green")
                                  (map second)
                                  (map read-string)
                                  (apply max))
                  max-blues (->> line
                                 (re-seq #"(\d+) blue")
                                 (map second)
                                 (map read-string)
                                 (apply max))]
              (+ a (* max-reds max-greens max-blues))))
          0
          (clojure.string/split-lines input)))

(comment
  (solve-a input)
  ;; 3035

  (solve-b input)
  ;; 66027
  )

