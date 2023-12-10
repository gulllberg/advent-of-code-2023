(ns advent-of-code-2023.day-10
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day10.txt"))
(def test-input "7-F7-\n.FJ|7\nSJLL7\n|F--J\nLJ.LJ")

(defn get-start-position
  [input]
  (let [lines (clojure.string/split-lines input)]
    (reduce (fn [_ i]
              (let [line (nth lines i)]
                (when-let [v (reduce (fn [_ j]
                                       (when (= (nth line j) \S)
                                         (reduced [i j])))
                                     nil
                                     (range (count line)))]
                  (reduced v))))
            nil
            (range (count lines)))))

(defn make-state
  [input]
  (let [lines (clojure.string/split-lines input)]
    (reduce (fn [a i]
              (let [line (nth lines i)]
                (reduce (fn [a j]
                          (assoc a [i j] (nth line j)))
                        a
                        (range (count line)))))
            {}
            (range (count lines)))))

(defn get-next-position
  [state position prev-position]
  (let [[m1 m2] (condp = (get state position)
                  \| [[1 0] [-1 0]]
                  \- [[0 1] [0 -1]]
                  \L [[-1 0] [0 1]]
                  \J [[-1 0] [0 -1]]
                  \7 [[1 0] [0 -1]]
                  \F [[1 0] [0 1]])
        p1 (mapv + position m1)
        p2 (mapv + position m2)]
    (if (= p1 prev-position)
      p2
      p1)))

(defn traverse-pipes
  [state start]
  ;; Start by walking down because it fits test and real input
  (loop [position (mapv + start [1 0])
         prev-position start
         visited #{start}]
    (if (= position start)
      visited
      (recur (get-next-position state position prev-position)
             position
             (conj visited position)))))


(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 8))}
  [input]
  (let [state (make-state input)
        start (get-start-position input)]
    (/ (count (traverse-pipes state start)) 2)))

(def test-input-2 "..........\n.S------7.\n.|F----7|.\n.||OOOO||.\n.||OOOO||.\n.|L-7F-J|.\n.|II||II|.\n.L--JL--J.\n..........")
(def test-input-3 ".F----7F7F7F7F-7....\n.|F--7||||||||FJ....\n.||.FJ||||||||L7....\nFJL7L7LJLJ||LJ.L-7..\nL--J.L7...LJS7F-7L7.\n....F-J..F7FJ|L7L7L7\n....L7.F7||L7|.L7L7|\n.....|FJLJ|FJ|F7|.LJ\n....FJL-7.||.||||...\n....L---J.LJ.LJLJ...")
(def test-input-4 "FF7FSF7F7F7F7F7F---7\nL|LJ||||||||||||F--J\nFL-7LJLJ||||||LJL-77\nF--JF--7||LJLJ7F7FJ-\nL---JF-JLJ.||-FJLJJ7\n|F|F-JF---7F7-L7L|7|\n|FFJF7L7F-JF7|JL---7\n7-L-JL7||F7|L7F-7F7|\nL.L7LFJ|||||FJL7||LJ\nL7JLJL-JLJLJL--JLJ.L")

(defn replace-start-pipe
  [state start-pipe-type]
  (reduce-kv (fn [m k v]
               (assoc m k (if (= v \S) start-pipe-type v)))
             {}
             state))

(def pipe-type->directions {\| #{[-1 0] [0 0] [1 0]}
                            \- #{[0 -1] [0 0] [0 1]}
                            \L #{[-1 0] [0 0] [0 1]}
                            \J #{[0 -1] [0 0] [-1 0]}
                            \7 #{[1 0] [0 0] [0 -1]}
                            \F #{[1 0] [0 0] [0 1]}})

(def directions #{[-1 -1] [-1 0] [-1 1]
                  [0 -1] [0 0] [0 1]
                  [1 -1] [1 0] [1 1]})

(defn expand-pipe-system
  [state pipe-positions]
  (reduce (fn [m [i j]]
            (let [center-point [(* i 3) (* j 3)]
                  pipe-type (and (contains? pipe-positions [i j]) (get state [i j]))
                  pipe-directions (get pipe-type->directions pipe-type #{})]
              (reduce (fn [m d]
                        (assoc m (mapv + center-point d) (contains? pipe-directions d)))
                      m
                      directions)))
          {}
          (keys state)))

(defn get-all-non-pipe-positions
  [state pipe-positions]
  (reduce (fn [a pos]
            (if (contains? pipe-positions pos)
              a
              (conj a pos)))
          #{}
          (keys state)))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input \F) 1)
           (is= (solve-b test-input-2 \F) 4)
           (is= (solve-b test-input-3 \F) 8)
           (is= (solve-b test-input-4 \7) 10))}
  [input start-pipe-type]
  (let [state (make-state input)
        start (get-start-position input)
        pipe-positions (traverse-pipes state start)
        state (replace-start-pipe state start-pipe-type)
        expanded-state (expand-pipe-system state pipe-positions)
        starting-positions (get-all-non-pipe-positions state pipe-positions)]
    (last (reduce (fn [[outside inside number-of-inside] [i j]]
                    (let [transformed-starting-position [(* 3 i) (* 3 j)]
                          [outside inside was-inside] (loop [position transformed-starting-position
                                                             potential #{}
                                                             visited #{transformed-starting-position}]
                                                        (cond
                                                          (or (not (contains? expanded-state position))
                                                              (contains? outside position))
                                                          [(clojure.set/union outside visited potential) inside false]

                                                          (contains? inside position)
                                                          [outside (clojure.set/union inside visited potential) true]

                                                          :else
                                                          (let [new-positions (->> directions
                                                                                   (map (fn [d]
                                                                                          (mapv + position d)))
                                                                                   (remove (fn [p]
                                                                                             (or (contains? visited p)
                                                                                                 (get expanded-state p)))))
                                                                new-potential (reduce conj potential new-positions)
                                                                new-position (first new-potential)]
                                                            (if new-position
                                                              (recur new-position (disj new-potential new-position) (conj visited new-position))
                                                              [outside (clojure.set/union inside visited) true]))))]
                      (if was-inside
                        [outside inside (inc number-of-inside)]
                        [outside inside number-of-inside])))
                  [#{} #{} 0]
                  starting-positions))))

(comment
  (time (solve-a input))
  ;; 6786
  ;; "Elapsed time: 14.919167 msecs"

  (time (solve-b input \|))
  ;; 495
  ;; "Elapsed time: 757.9455 msecs"
  )

