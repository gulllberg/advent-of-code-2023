(ns advent-of-code-2023.day-14
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day14.txt"))
(def test-input "O....#....\nO.OO#....#\n.....##...\nOO.#O....O\n.O.....O#.\nO.#..O.#.#\n..O..#O..O\n.......O..\n#....###..\n#OO..#....")

(defn make-state
  [input]
  (let [lines (clojure.string/split-lines input)]
    (reduce (fn [a i]
              (let [line (nth lines i)]
                (reduce (fn [a j]
                          (assoc a [i j] (condp = (nth line j)
                                           \# :square
                                           \O :round
                                           \. :air)))
                        a
                        (range (count line)))))
            {}
            (range (count lines)))))

(defn draw-state
  [state]
  (println "")
  (loop [i 0
         j 0
         row ""]
    (cond
      (and (= j 0)
           (not (get state [i j])))
      (println "")

      (not (get state [i j]))
      (do (println row)
          (recur (inc i) 0 ""))

      :else
      (recur i (inc j) (str row (condp = (get state [i j])
                                  :square "#"
                                  :round "O"
                                  :air "."))))))

(defn roll-one-rock-north
  [state i j]
  (if (not= (get state [i j]) :round)
    state
    (reduce (fn [state i]
              (if (not= (get state [(dec i) j]) :air)
                (reduced state)
                (-> state
                    (assoc [i j] :air)
                    (assoc [(dec i) j] :round))))
            state
            (range i 0 -1))))

(defn roll-north
  [state]
  (loop [i 0
         j 0
         state state]
    (cond
      (and (= j 0)
           (not (get state [i j])))
      state

      (not (get state [i j]))
      (recur (inc i) 0 state)

      :else
      (recur i (inc j) (roll-one-rock-north state i j)))))

(defn count-rock-load
  [state]
  (let [number-of-rows (->> state
                            (keys)
                            (map first)
                            (apply max)
                            (inc))]
    (reduce-kv (fn [a [i _] v]
                 (if (not= v :round)
                   a
                   (+ a (- number-of-rows i))))
               0
               state)))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 136))}
  [input]
  (let [state (make-state input)
        rolled-state (roll-north state)]
    (count-rock-load rolled-state)))

(defn roll-one-loose-rock
  [state loose-rocks i j dir]
  (let [next-pos (mapv + [i j] dir)]
    (condp = (get state next-pos)
      nil
      [state false [i j]]

      :square
      [state false [i j]]

      :air
      [(-> state
           (assoc [i j] :air)
           (assoc next-pos :round))
       true
       next-pos]

      :round
      [state (contains? loose-rocks next-pos) [i j]])))

(defn roll-all-loose-rocks
  [state loose-rocks dir]
  (reduce (fn [[state still-loose-rocks] [i j]]
            (let [[state still-loose new-pos] (roll-one-loose-rock state loose-rocks i j dir)]
              (if still-loose
                [state (conj still-loose-rocks new-pos)]
                [state still-loose-rocks])))
          [state #{}]
          loose-rocks))

(defn get-all-loose-rocks
  [state]
  (reduce-kv (fn [a k v]
               (if (= v :round)
                 (conj a k)
                 a))
             #{}
             state))

(defn roll-in-direction
  [state dir]
  (loop [state state
         loose-rocks (get-all-loose-rocks state)]
    (let [[next-state still-loose-rocks] (roll-all-loose-rocks state loose-rocks dir)]
      (if (empty? still-loose-rocks)
        next-state
        (recur next-state still-loose-rocks)))))

(defn roll-cycle
  [state]
  (reduce roll-in-direction
          state
          [[-1 0] [0 -1] [1 0] [0 1]]))

(defn get-answer-from-cycle-detection
  [states n first-occurrence]
  (let [cycle-length (- n first-occurrence)
        steps-to-go (mod (- 1000000000 n) cycle-length)
        target-first-occurrence (+ first-occurrence steps-to-go)]
    (count-rock-load (reduce-kv (fn [_ k v]
                                  (if (= v target-first-occurrence)
                                    (reduced k)
                                    nil))
                                nil
                                states))))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 64))}
  [input]
  (let [state (make-state input)]
    (loop [state state
           states {state 0}
           n 0]
      (let [next-n (inc n)
            next-state (roll-cycle state)]
        (if-let [first-occurrence (get states next-state)]
          (get-answer-from-cycle-detection states next-n first-occurrence)
          (recur next-state (assoc states next-state next-n) next-n))))))

(comment
  (time (solve-a input))
  ;; 109424
  ;; "Elapsed time: 35.420582 msecs"

  (time (solve-b input))
  ;; 102509
  ;; "Elapsed time: 12500.013893 msecs"

  )
