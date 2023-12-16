(ns advent-of-code-2023.day-16
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day16.txt"))
(def test-input ".|...\\....\n|.-.\\.....\n.....|-...\n........|.\n..........\n.........\\\n..../.\\\\..\n.-.-/..|..\n.|....-|.\\\n..//.|....")

(defn make-state
  [input]
  (let [lines (clojure.string/split-lines input)]
    (reduce (fn [a i]
              (let [line (nth lines i)]
                (reduce (fn [a j]
                          (let [c (nth line j)]
                            (-> (if (= c \.)
                                  a
                                  (update a :mirrors assoc [i j] c))
                                (update :max-i max i)
                                (update :max-j max j))))
                        a
                        (range (count line)))))
            {:mirrors {}
             :max-i   0
             :max-j   0}
            (range (count lines)))))

(defn get-new-directions
  [dir mirror]
  (cond
    ;; Pass through splitter unaffected
    (and (= mirror \-)
         (contains? #{[0 1] [0 -1]} dir))
    [dir]
    (and (= mirror \|)
         (contains? #{[1 0] [-1 0]} dir))
    [dir]

    ;; Reflect down
    (and (= mirror \\)
         (= dir [0 1]))
    [[1 0]]
    (and (= mirror \/)
         (= dir [0 -1]))
    [[1 0]]

    ;; Reflect up
    (and (= mirror \\)
         (= dir [0 -1]))
    [[-1 0]]
    (and (= mirror \/)
         (= dir [0 1]))
    [[-1 0]]

    ;; Reflect right
    (and (= mirror \\)
         (= dir [1 0]))
    [[0 1]]
    (and (= mirror \/)
         (= dir [-1 0]))
    [[0 1]]

    ;; Reflect left
    (and (= mirror \\)
         (= dir [-1 0]))
    [[0 -1]]
    (and (= mirror \/)
         (= dir [1 0]))
    [[0 -1]]

    ;; Up/down split
    (and (= mirror \|)
         (contains? #{[0 1] [0 -1]} dir))
    [[1 0] [-1 0]]

    ;; Right/left split
    (and (= mirror \-)
         (contains? #{[1 0] [-1 0]} dir))
    [[0 1] [0 -1]]))

(defn inside-room?
  [[i j] max-i max-j]
  (and (<= 0 i max-i)
       (<= 0 j max-j)))

(defn get-new-beams-and-exited
  [state visited [pos dir]]
  (let [mirror (get-in state [:mirrors pos])
        new-beams (if-not mirror
                    [[(mapv + pos dir) dir]]
                    (reduce (fn [a d]
                              (conj a [(mapv + pos d) d]))
                            []
                            (get-new-directions dir mirror)))
        new-beams (remove (fn [beam]
                            (contains? visited beam))
                          new-beams)]
    [(->> new-beams
          (filter (fn [[pos _]]
                    (inside-room? pos (:max-i state) (:max-j state))))
          (into #{}))
     (->> new-beams
          (remove (fn [[pos _]]
                    (inside-room? pos (:max-i state) (:max-j state))))
          (map first)
          (into #{}))]))

(defn energize
  [state [pos dir]]
  (loop [beams #{[(mapv + pos dir) dir]}
         visited #{[(mapv + pos dir) dir]}
         exited #{}]
    (if (empty? beams)
      [visited exited]
      (let [beam (first beams)
            [new-beams new-exited] (get-new-beams-and-exited state visited beam)]
        (recur (clojure.set/union (disj beams beam) new-beams)
               (clojure.set/union visited new-beams)
               (clojure.set/union exited new-exited))))))

(defn count-energized
  [visited]
  (->> visited
       (map first)
       (into #{})
       (count)))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 46))}
  [input]
  (let [state (make-state input)
        [visited _] (energize state [[0 -1] [0 1]])]
    (count-energized visited)))

(defn get-all-start-beams
  [max-i max-j]
  (let [left-side (reduce (fn [a i]
                            (conj a [[i -1] [0 1]]))
                          #{}
                          (range 0 (inc max-i)))
        right-side (reduce (fn [a i]
                             (conj a [[i (inc max-j)] [0 -1]]))
                           #{}
                           (range 0 (inc max-i)))
        top-side (reduce (fn [a j]
                           (conj a [[-1 j] [1 0]]))
                         #{}
                         (range 0 (inc max-j)))
        bottom-side (reduce (fn [a j]
                              (conj a [[(inc max-i) j] [-1 0]]))
                            #{}
                            (range 0 (inc max-j)))]
    (clojure.set/union left-side right-side top-side bottom-side)))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 51))}
  [input]
  (let [state (make-state input)
        all-start-beams (get-all-start-beams (:max-i state) (:max-j state))]
    (loop [start-beams all-start-beams
           best 0]
      (if (empty? start-beams)
        best
        (let [start-beam (first start-beams)
              [visited exited] (energize state start-beam)
              res (count-energized visited)
              new-start-beams (into #{} (remove (fn [[pos _]]
                                                  (contains? exited pos))
                                                (disj start-beams start-beam)))]
          (recur new-start-beams
                 (max best res)))))))

(comment
  (time (solve-a input))
  ;; 7067
  ;; "Elapsed time: 18.029333 msecs"

  (time (solve-b input))
  ;; 7324
  ;; "Elapsed time: 3158.184292 msecs"

  )
