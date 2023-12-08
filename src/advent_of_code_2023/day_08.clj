(ns advent-of-code-2023.day-08
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day08.txt"))
(def test-input "RL\n\nAAA = (BBB, CCC)\nBBB = (DDD, EEE)\nCCC = (ZZZ, GGG)\nDDD = (DDD, DDD)\nEEE = (EEE, EEE)\nGGG = (GGG, GGG)\nZZZ = (ZZZ, ZZZ)")

(defn get-desert-map
  [input]
  (as-> input $
        (clojure.string/split $ #"\n\n")
        (second $)
        (clojure.string/split-lines $)
        (reduce (fn [a line]
                  (let [[node left right] (re-seq #"\w+" line)]
                    (assoc a node {"L" left "R" right})))
                {}
                $)))

(defn get-left-right-instructions
  [input]
  (-> input
      (clojure.string/split #"\n\n")
      (first)
      (clojure.string/split #"")
      (cycle)))

(defn find-exit
  [desert-map instructions start ends]
  (loop [node start
         [instruction & rest-instructions] instructions
         number-of-steps 0]
    (if (and (not (zero? number-of-steps)) (contains? ends node))
      [number-of-steps node]
      (recur (get-in desert-map [node instruction])
             rest-instructions
             (inc number-of-steps)))))

(defn solve-a
  {:test (fn [] (is= (solve-a test-input) 2))}
  [input]
  (let [left-right-instructions (get-left-right-instructions input)
        desert-map (get-desert-map input)]
    (first (find-exit desert-map left-right-instructions "AAA" #{"ZZZ"}))))

(def test-input-2 "LR\n\n11A = (11B, XXX)\n11B = (XXX, 11Z)\n11Z = (11B, XXX)\n22A = (22B, XXX)\n22B = (22C, 22C)\n22C = (22Z, 22Z)\n22Z = (22B, 22B)\nXXX = (XXX, XXX)")

(defn get-starting-nodes
  [desert-map]
  (->> desert-map
       (keys)
       (filter (fn [k]
                 (clojure.string/ends-with? k "A")))))

(defn get-end-nodes
  [desert-map]
  (->> desert-map
       (keys)
       (filter (fn [k]
                 (clojure.string/ends-with? k "Z")))
       (into #{})))

(defn get-node-cycle
  [desert-map instructions node ends]
  (let [[cycle-start end] (find-exit desert-map instructions node ends)
        [cycle-length _] (find-exit desert-map (drop cycle-start instructions) end #{end})]
    [cycle-start cycle-length]))

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (if (or (zero? a) (zero? b))
    0
    (/ (abs (* a b)) (gcd a b))))

(defn lcm-of-list [numbers]
  (if (empty? numbers)
    0
    (let [common-divisor (reduce gcd numbers)]
      (* common-divisor (reduce (fn [acc n] (lcm acc (/ n common-divisor))) 1 numbers)))))

(defn solve-b
  {:test (fn [] (is= (solve-b test-input-2) 6))}
  [input]
  (let [left-right-instructions (get-left-right-instructions input)
        desert-map (get-desert-map input)
        starting-nodes (get-starting-nodes desert-map)
        end-nodes (get-end-nodes desert-map)
        cycle-lengths (map (fn [starting-node]
                             ;; For all cycles the first occurrence is equal to the cycle length
                             (first (get-node-cycle desert-map left-right-instructions starting-node end-nodes)))
                           starting-nodes)]
    (lcm-of-list cycle-lengths)))

(comment
  (time (solve-a input))
  ;; 11567
  ;; "Elapsed time: 8.506875 msecs"

  (time (solve-b input))
  ;; 9858474970153
  ;; "Elapsed time: 26.533833 msecs"
  )

