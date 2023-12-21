(ns advent-of-code-2023.day-21
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day21.txt"))
(def test-input "...........\n.....###.#.\n.###.##..#.\n..#.#...#..\n....#.#....\n.##..S####.\n.##..#...#.\n.......##..\n.##.#.####.\n.##..##.##.\n...........")

(defn create-state
  [input]
  (let [lines (clojure.string/split-lines input)]
    (reduce (fn [[a start] i]
              (let [line (nth lines i)]
                (reduce (fn [[a start] j]
                          (condp = (nth line j)
                            \S [(conj a [i j]) [i j]]
                            \. [(conj a [i j]) start]
                            [a start]))
                        [a start]
                        (range (count line)))))
            [#{} nil]
            (range (count lines)))))

(defn get-neighbours
  [state visited pos]
  (reduce (fn [a d]
            (let [next-pos (mapv + pos d)]
              (if (and (contains? state next-pos)
                       (not (contains? visited next-pos)))
                (conj a next-pos)
                a)))
          #{}
          [[1 0] [-1 0] [0 1] [0 -1]]))

(defn get-possible-end-positions
  [state start-pos num-steps]
  (loop [perimeter #{start-pos}
         even-visited #{start-pos}
         odd-visited #{}
         steps 0]
    (if (or (= steps num-steps)
            (empty? perimeter))
      [even-visited odd-visited]
      (let [next-step (inc steps)
            visited (if (even? next-step) even-visited odd-visited)
            new-perimeter (reduce (fn [a p]
                                    (clojure.set/union a (get-neighbours state visited p)))
                                  #{}
                                  perimeter)]
        (recur new-perimeter
               (if (even? next-step) (clojure.set/union even-visited new-perimeter) even-visited)
               (if (odd? next-step) (clojure.set/union odd-visited new-perimeter) odd-visited)
               next-step)))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input 6) 16)
           )}
  [input num-steps]
  (let [[state start-pos] (create-state input)
        [even-visited _] (get-possible-end-positions state start-pos num-steps)]
    (count even-visited)))

(defn solve-b
  [input]
  (let [[state start-pos] (create-state input)
        ;; Max number of steps is 202300*131+65. Grid is 131x131
        ;; There is a straight path from start to the edges, and around the edges.
        ;; Hence, in 202300x131 steps you can get to the "perimeter"
        ;; Everything here and inside can be reached (depending on odd/even number of steps)
        ;; From the perimeter there are 3 kinds of bordering squares.
        ;; They are handled, considering symmetry in 4 directions.
        ;; Number of interior and perimeter squares is arithmetic series.

        [even-visited-interior odd-visited-interior] (get-possible-end-positions state start-pos ##Inf)
        num-even-interior (/ (* 4 101150 (+ 1 202299)) 2)
        num-odd-interior (inc (/ (* 4 101150 (+ 0 202298)) 2))
        num-perimeter (* 4 202299)

        num-perimeter-outer-corner (/ (+ 4 num-perimeter) 4)
        [even-visited-perimeter-outer-corner-1 _] (get-possible-end-positions state [0 0] 64)
        [even-visited-perimeter-outer-corner-2 _] (get-possible-end-positions state [130 0] 64)
        [even-visited-perimeter-outer-corner-3 _] (get-possible-end-positions state [0 130] 64)
        [even-visited-perimeter-outer-corner-4 _] (get-possible-end-positions state [130 130] 64)

        num-perimeter-inner-corner (dec num-perimeter-outer-corner)
        [_ odd-visited-perimeter-inner-corner-1] (get-possible-end-positions state [0 0] 195)
        [_ odd-visited-perimeter-inner-corner-2] (get-possible-end-positions state [130 0] 195)
        [_ odd-visited-perimeter-inner-corner-3] (get-possible-end-positions state [0 130] 195)
        [_ odd-visited-perimeter-inner-corner-4] (get-possible-end-positions state [130 130] 195)

        ; num-perimeter-middle 1
        [even-visited-perimeter-middle-1 _] (get-possible-end-positions state [0 65] 130)
        [even-visited-perimeter-middle-2 _] (get-possible-end-positions state [130 65] 130)
        [even-visited-perimeter-middle-3 _] (get-possible-end-positions state [65 0] 130)
        [even-visited-perimeter-middle-4 _] (get-possible-end-positions state [65 130] 130)]
    (+ (* num-even-interior (count even-visited-interior))
       (* num-odd-interior (count odd-visited-interior))
       (* num-perimeter-outer-corner (count even-visited-perimeter-outer-corner-1))
       (* num-perimeter-outer-corner (count even-visited-perimeter-outer-corner-2))
       (* num-perimeter-outer-corner (count even-visited-perimeter-outer-corner-3))
       (* num-perimeter-outer-corner (count even-visited-perimeter-outer-corner-4))
       (* num-perimeter-inner-corner (count odd-visited-perimeter-inner-corner-1))
       (* num-perimeter-inner-corner (count odd-visited-perimeter-inner-corner-2))
       (* num-perimeter-inner-corner (count odd-visited-perimeter-inner-corner-3))
       (* num-perimeter-inner-corner (count odd-visited-perimeter-inner-corner-4))
       (count even-visited-perimeter-middle-1)
       (count even-visited-perimeter-middle-2)
       (count even-visited-perimeter-middle-3)
       (count even-visited-perimeter-middle-4))))

(comment
  (time (solve-a input 64))
  ;; 3651
  ;; "Elapsed time: 20.655292 msecs"

  (time (solve-b input))
  ;; 607334325965751
  ;; "Elapsed time: 321.046541 msecs"
  )
