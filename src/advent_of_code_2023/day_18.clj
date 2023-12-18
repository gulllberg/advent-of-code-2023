(ns advent-of-code-2023.day-18
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day18.txt"))
(def test-input "R 6 (#70c710)\nD 5 (#0dc571)\nL 2 (#5713f0)\nD 2 (#d2c081)\nR 2 (#59c680)\nD 2 (#411b91)\nL 5 (#8ceee2)\nU 2 (#caa173)\nL 1 (#1b58a2)\nU 2 (#caa171)\nR 2 (#7807d2)\nU 3 (#a77fa3)\nL 2 (#015232)\nU 2 (#7a21e3)\n")

(defn parse-line
  {:test (fn []
           (is= (parse-line "R 6 (#70c710)") ["R" 6 "70c710"]))}
  [line]
  (let [[p1 p2 p3] (clojure.string/split line #" ")]
    [p1 (read-string p2) (first (re-find #"(\d|[a-z])+" p3))]))

(defn dir->dir
  [dir]
  (condp = dir
    "R" [0 1]
    "L" [0 -1]
    "U" [-1 0]
    "D" [1 0]))

(defn fill-interior
  [dig-positions]
  ;; start position is inside for this data
  (loop [positions-to-test [[1 1]]
         interior dig-positions]
    (if (empty? positions-to-test)
      interior
      (let [[position-to-test & rest] positions-to-test
            new-positions (reduce (fn [a d]
                                    (let [new-pos (map + position-to-test d)]
                                      (if (contains? interior new-pos)
                                        a
                                        (conj a new-pos)))
                                    )
                                  #{}
                                  (map dir->dir ["R" "L" "D" "U"]))]
        (recur (concat rest (into [] new-positions)) (clojure.set/union interior new-positions))))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 62))}
  [input]
  (->> (clojure.string/split-lines input)
       (map parse-line)
       (reduce (fn [[pos a] [dir steps _]]
                 (reduce (fn [[pos a] _]
                           (let [new-pos (map + pos (dir->dir dir))]
                             [new-pos (conj a new-pos)]))
                         [pos a]
                         (range steps)))
               [[0 0] #{}])
       (second)
       (fill-interior)
       (count)))

(defn dir->dir-b
  [dir]
  (condp = dir
    "0" [0 1]
    "2" [0 -1]
    "3" [-1 0]
    "1" [1 0]))

(defn count-interior
  [corners]
  (let [sorted-corners (sort (fn [a b]
                               (let [f-a (first a)
                                     f-b (first b)]
                                 (if (= f-a f-b)
                                   (< (second a) (second b))
                                   (< f-a f-b))))
                         corners)]
    sorted-corners
    (loop [n 0
           corners sorted-corners]
      (let [i (ffirst corners)
            on-current-row (take-while (fn [c]
                                         (= (first c) i))
                                       corners)
            the-rest (drop-while (fn [c]
                                   (= (first c) i))
                                 corners)]
        (reduce (fn [a [[_ j1] [_ j2]]]
                  (+ a (- j2 j1)))
                0
                (partition 2 on-current-row)) ))))



(defn hej
  {:test (fn []
           (is= (hej test-input) 62))}
  [input]
  (->> (clojure.string/split-lines input)
       (map parse-line)
       (reduce (fn [[pos a] [dir steps _]]
                 (let [new-pos (map + pos (map * (dir->dir dir) [steps steps]))]
                   [new-pos (conj a new-pos)]))
               [[0 0] []])
       (second)
       (count-interior)))

(defn solve-b
  ;{:test (fn []
  ;         (is= (solve-b test-input) 952408144115))}
  [input]
  (->> (clojure.string/split-lines input)
       (map parse-line)
       (reduce (fn [[pos a] [_ _ colour]]
                 (let [steps (Integer/parseInt (subs colour 0 5) 16)
                       dir (dir->dir-b (subs colour 5))
                       new-pos (map + pos (map * dir [steps steps]))]
                   [new-pos (conj a new-pos)]))
               [[0 0] []])
       (second)
       (sort (fn [a b]
               (< (first a) (first b))))
       (count-interior)))

(comment
  (time (solve-a input))
  ;; 66993
  ;; "Elapsed time: 2803.676581 msecs"

  (time (solve-b input))
  ;;
  ;;
  )
