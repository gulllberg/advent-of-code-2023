(ns advent-of-code-2023.day-09
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day09.txt"))
(def test-input "0 3 6 9 12 15\n1 3 6 10 15 21\n10 13 16 21 30 45")

(defn get-differences
  [numbers]
  (reduce (fn [a i]
            (conj a (- (nth numbers (inc i))
                       (nth numbers i))))
          []
          (range (dec (count numbers)))))

(defn get-all-differences
  [history]
  (loop [res [history]]
    (let [numbers (last res)]
      (if (every? zero? numbers)
        res
        (recur (conj res (get-differences numbers)))))))

(defn get-next-number
  [all-differences]
  (reduce + (map last all-differences)))

(defn solve-a
  {:test (fn [] (is= (solve-a test-input) 114))}
  [input]
  (let [histories (->> input
                       (clojure.string/split-lines)
                       (map (fn [line]
                              (into [] (map read-string (clojure.string/split line #" "))))))]
    (->> histories
         (map (comp get-next-number get-all-differences))
         (reduce +))))

(defn get-previous-number
  [all-differences]
  (reduce (fn [a differences]
            (- (first differences) a))
          0
          (reverse (drop-last all-differences))))

(defn solve-b
  {:test (fn [] (is= (solve-b test-input) 2))}
  [input]
  (let [histories (->> input
                       (clojure.string/split-lines)
                       (map (fn [line]
                              (into [] (map read-string (clojure.string/split line #" "))))))]
    (->> histories
         (map (comp get-previous-number get-all-differences))
         (reduce +))))

(comment
  (time (solve-a input))
  ;; 1666172641
  ;; "Elapsed time: 6.395583 msecs"

  (time (solve-b input))
  ;; 933
  ;; "Elapsed time: 7.930041 msecs"
  )

