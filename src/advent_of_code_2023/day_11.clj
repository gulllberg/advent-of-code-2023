(ns advent-of-code-2023.day-11
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day11.txt"))
(def test-input "...#......\n.......#..\n#.........\n..........\n......#...\n.#........\n.........#\n..........\n.......#..\n#...#.....")

(defn get-rows-columns-without-galaxies
  [input]
  (let [lines (clojure.string/split-lines input)
        [all-rows rows-with-galaxies all-columns columns-with-galaxies] (reduce (fn [a i]
                                                                                  (let [line (nth lines i)]
                                                                                    (reduce (fn [[all-rows rows-with-galaxies all-columns columns-with-galaxies] j]
                                                                                              (let [c (nth line j)
                                                                                                    is-galaxy (= c \#)]
                                                                                                [(conj all-rows i) (if is-galaxy (conj rows-with-galaxies i) rows-with-galaxies)
                                                                                                 (conj all-columns j) (if is-galaxy (conj columns-with-galaxies j) columns-with-galaxies)]))
                                                                                            a
                                                                                            (range (count line)))))
                                                                                [#{} #{} #{} #{}]
                                                                                (range (count lines)))]
    [(clojure.set/difference all-rows rows-with-galaxies)
     (clojure.set/difference all-columns columns-with-galaxies)]))

(defn get-galaxy-positions
  [input expansion-factor]
  (let [[rows-without-galaxies columns-without-galaxies] (get-rows-columns-without-galaxies input)
        lines (clojure.string/split-lines input)]
    (first (reduce (fn [[a extra-rows] i]
                     (let [line (nth lines i)]
                       (if (contains? rows-without-galaxies i)
                         [a (inc extra-rows)]
                         [(first (reduce (fn [[a extra-columns] j]
                                           (let [c (nth line j)
                                                 is-galaxy (= c \#)]
                                             (cond
                                               (contains? columns-without-galaxies j)
                                               [a (inc extra-columns)]

                                               is-galaxy
                                               [(conj a [(+ i (* extra-rows (dec expansion-factor))) (+ j (* extra-columns (dec expansion-factor)))]) extra-columns]

                                               :else
                                               [a extra-columns])))
                                         [a 0]
                                         (range (count line))))
                          extra-rows])
                       ))
                   [[] 0]
                   (range (count lines))))))

(defn manhattan-distance
  [p1 p2]
  (+ (abs (- (first p1) (first p2)))
     (abs (- (second p1) (second p2)))))

(defn sum-galaxy-distances
  [galaxy-positions]
  (loop [[galaxy & other-galaxies] galaxy-positions
         total-distance 0]
    (if (empty? other-galaxies)
      total-distance
      (recur other-galaxies
             (reduce (fn [a p]
                       (+ a (manhattan-distance galaxy p)))
                     total-distance
                     other-galaxies)))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 374))}
  [input]
  (let [galaxy-positions (get-galaxy-positions input 2)]
    (sum-galaxy-distances galaxy-positions)))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input 10) 1030)
           (is= (solve-b test-input 100) 8410))}
  [input expansion-factor]
  (let [galaxy-positions (get-galaxy-positions input expansion-factor)]
    (sum-galaxy-distances galaxy-positions)))

(comment
  (time (solve-a input))
  ;; 9639160
  ;; "Elapsed time: 34.337034 msecs"

  (time (solve-b input 1000000))
  ;; 752936133304
  ;; "Elapsed time: 34.616014 msecs"
  )

