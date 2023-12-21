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

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input 6) 16)
           )}
  [input num-steps]
  (let [[state start-pos] (create-state input)]
    (loop [perimeter #{start-pos}
           even-visited #{start-pos}
           odd-visited #{}
           steps 0]
      (if (or (= steps num-steps)
              (empty? perimeter))
        (count even-visited)
        (let [next-step (inc steps)
              visited (if (even? next-step) even-visited odd-visited)
              new-perimeter (reduce (fn [a p]
                                      (clojure.set/union a (get-neighbours state visited p)))
                                    #{}
                                    perimeter)]
          (recur new-perimeter
                 (if (even? next-step) (clojure.set/union even-visited new-perimeter) even-visited)
                 (if (odd? next-step) (clojure.set/union odd-visited new-perimeter) odd-visited)
                 next-step))))))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input 6) 16)
           (is= (solve-b test-input 10) 50)
           (is= (solve-b test-input 50) 1594)
           (is= (solve-b test-input 100) 6536)
           (is= (solve-b test-input 500) 167004)
           (is= (solve-b test-input 1000) 668697)
           (is= (solve-b test-input 5000) 16733044))}
  [input num-steps]
  0)

(comment
  (time (solve-a input 64))
  ;; 3651
  ;; "Elapsed time: 20.655292 msecs"

  (time (solve-b input 26501365))
  ;;
  ;;
  )
