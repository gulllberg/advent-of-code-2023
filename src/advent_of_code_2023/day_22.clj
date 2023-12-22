(ns advent-of-code-2023.day-22
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day22.txt"))
(def test-input "1,0,1~1,2,1\n0,0,2~2,0,2\n0,2,3~2,2,3\n0,0,4~0,2,4\n2,0,5~2,2,5\n0,1,6~2,1,6\n1,1,8~1,1,9\n")

(defn create-state
  [input]
  (let [lines (clojure.string/split-lines input)
        [max-x max-y] (reduce (fn [[max-x max-y] line]
                                (let [[x1 y1 _ x2 y2 _] (map read-string (re-seq #"\d+" line))]
                                  [(max max-x x1 x2) (max max-y y1 y2)]))
                              [0 0]
                              lines)]
    (reduce (fn [a line]
              (let [[x1 y1 z1 x2 y2 z2] (map read-string (re-seq #"\d+" line))]
                (update a :pieces conj {:id    line
                                        :cells (into #{} (for [x (range x1 (inc x2))
                                                               y (range y1 (inc y2))
                                                               z (range z1 (inc z2))]
                                                           [x y z]))})))
            {:pieces         #{}
             :pieces-at-rest #{}
             :z-levels       (zipmap (for [x (range (inc max-x)) y (range (inc max-y))] [x y]) (repeat 0))}
            lines)))

(defn let-pieces-fall
  [state]
  {:post [(set? %)]}
  (loop [state (update state :pieces (fn [pieces]
                                       (sort (fn [a b]
                                               (let [max-a (apply max (map last (:cells a)))
                                                     max-b (apply max (map last (:cells b)))]
                                                 (compare max-a max-b)))
                                             pieces)))]
    (if (empty? (get state :pieces))
      (into #{} (get state :pieces-at-rest))
      (let [[piece & rest-pieces] (get state :pieces)
            z-level (reduce (fn [a [x y _]]
                              (max a (get-in state [:z-levels [x y]])))
                            0
                            (:cells piece))
            num-z-values (->> piece
                              (:cells)
                              (map last)
                              (distinct)
                              (count))
            fallen-piece (if (= num-z-values 1)
                           (update piece :cells (fn [cells]
                                                  (into #{} (map (fn [[x y _]]
                                                                   [x y (inc z-level)])
                                                                 cells))))
                           (update piece :cells (fn [cells]
                                                  (let [[x y _] (first cells)]
                                                    (reduce (fn [a z-inc]
                                                              (conj a [x y (+ z-level 1 z-inc)]))
                                                            #{}
                                                            (range num-z-values))))))]
        (recur {:pieces         rest-pieces
                :pieces-at-rest (conj (get state :pieces-at-rest) fallen-piece)
                :z-levels       (reduce (fn [a [x y _]]
                                          (assoc a [x y] (+ z-level num-z-values)))
                                        (get state :z-levels)
                                        (:cells fallen-piece))})))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 5))}
  [input]
  (let [state (create-state input)
        fallen-pieces (let-pieces-fall state)]
    (reduce (fn [a p]
              (println "testing" a)
              (let [test-fallen-pieces (let-pieces-fall {:pieces        (disj fallen-pieces p)
                                                         :fallen-pieces #{}
                                                         :z-levels      (get state :z-levels)})]
                (if (= (conj test-fallen-pieces p) fallen-pieces)
                  (inc a)
                  a)))
            0
            fallen-pieces)))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 7))}
  [input]
  (let [state (create-state input)
        fallen-pieces (let-pieces-fall state)]
    (reduce (fn [a p]
              (println "testing" a)
              (let [test-fallen-pieces (let-pieces-fall {:pieces        (disj fallen-pieces p)
                                                         :fallen-pieces #{}
                                                         :z-levels      (get state :z-levels)})]
                (+ a (count (clojure.set/difference (conj test-fallen-pieces p) fallen-pieces)))))
            0
            fallen-pieces)))

(comment
  (time (solve-a input))
  ;; 495
  ;; "Elapsed time: 28641.563584 msecs"

  (time (solve-b input))
  ;; 76158
  ;; "Elapsed time: 30629.869709 msecs"
  )
