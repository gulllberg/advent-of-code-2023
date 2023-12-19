(ns advent-of-code-2023.day-05
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day05.txt"))
(def test-input "seeds: 79 14 55 13\n\nseed-to-soil map:\n50 98 2\n52 50 48\n\nsoil-to-fertilizer map:\n0 15 37\n37 52 2\n39 0 15\n\nfertilizer-to-water map:\n49 53 8\n0 11 42\n42 0 7\n57 7 4\n\nwater-to-light map:\n88 18 7\n18 25 70\n\nlight-to-temperature map:\n45 77 23\n81 45 19\n68 64 13\n\ntemperature-to-humidity map:\n0 69 1\n1 0 69\n\nhumidity-to-location map:\n60 56 37\n56 93 4\n")

(defn get-mapping-fn
  [mapping]
  (let [[dest src len] (->> (re-seq #"\d+" mapping)
                            (map read-string))]
    (fn [x]
      (when (<= src x (+ src (dec len)))
        (+ x (- dest src))))))

(defn combine-mapping-fns
  [almanac]
  (let [parts (clojure.string/split-lines almanac)
        fns (mapv get-mapping-fn (rest parts))]
    (fn [x]
      (or (some identity (map (fn [mapping-fn]
                                (mapping-fn x))
                              fns))
          x))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 35))}
  [input]
  (let [almanac (clojure.string/split input #"\n\n")
        seeds (->> (re-seq #"\d+" (first almanac))
                   (map read-string))
        mapping-fns (map combine-mapping-fns (rest almanac))]
    (->> (map (fn [seed]
                (reduce (fn [a mapping-fn]
                          (mapping-fn a))
                        seed
                        mapping-fns))
              seeds)
         (apply min))))

;; e exclusive, 1,5 ->1,2,3,4
(defn interval-intersection
  {:test (fn []
           (is= (interval-intersection [1 5] [6 9]) [nil [1 5]])
           (is= (interval-intersection [1 5] [-1 0]) [nil [1 5]])
           (is= (interval-intersection [3 5] [1 5]) [[3 5]])
           (is= (interval-intersection [3 5] [4 6]) [[4 5] [3 4]])
           (is= (interval-intersection [3 5] [1 4]) [[3 4] [4 5]])
           (is= (interval-intersection [1 5] [2 4]) [[2 4] [1 2] [4 5]]))}
  [[s1 e1] [s2 e2]]
  ; [[intersecting-interval] [remaining-intervals (1 or 2)]]
  (cond
    ; i2 smaller than i2
    (> s1 e2)
    [nil [s1 e1]]

    ; i1 smaller than i2
    (> s2 e1)
    [nil [s1 e1]]

    ; i1 entirely within i2
    (<= s2 s1 e1 e2)
    [[s1 e1]]

    ; i1 intersects "to the left"
    (<= s1 s2 e1 e2)
    [[s2 e1] [s1 s2]]

    ; i1 intersects "to the right"
    (<= s2 s1 e2 e1)
    [[s1 e2] [e2 e1]]

    ; i2 entirely within i1 - split it in 3
    :else
    [[s2 e2] [s1 s2] [e2 e1]]))

(defn get-mapping-fn-b
  [mapping]
  (let [[dest src len] (->> (re-seq #"\d+" mapping)
                            (map read-string))
        [s2 e2] [src (+ src len)]]
    (fn [[s1 e1]]
      (let [[intersection & new-intervals] (interval-intersection [s1 e1] [s2 e2])]
        (if intersection
          (concat (mapv + intersection (into [] (repeat 2 (- dest src)))) new-intervals)
          (concat [nil] new-intervals))))))

(defn combine-mapping-fns-b
  [almanac]
  (let [parts (clojure.string/split-lines almanac)
        fns (mapv get-mapping-fn-b (rest parts))]
    (fn [[s e]]
      (let [[mapped-intervals unmapped-intervals] (reduce (fn [[mapped-intervals remaining-intervals] mapping-fn]
                                                            (if (empty? remaining-intervals)
                                                              (reduced mapped-intervals)
                                                              (reduce (fn [[mapped-intervals remaining-intervals] [s1 e1]]
                                                                        (let [[mapping & new-intervals] (mapping-fn [s1 e1])]
                                                                          (if mapping
                                                                            [(conj mapped-intervals mapping) (concat remaining-intervals new-intervals)]
                                                                            [mapped-intervals (concat remaining-intervals new-intervals)])))
                                                                      [mapped-intervals []]
                                                                      remaining-intervals)))
                                                          [[] [[s e]]]
                                                          fns)]
        (concat mapped-intervals unmapped-intervals)))))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 46))}
  [input]
  (let [almanac (clojure.string/split input #"\n\n")
        seed-partitions (->> (re-seq #"\d+" (first almanac))
                             (map read-string)
                             (partition 2))
        mapping-fns (map combine-mapping-fns-b (rest almanac))]
    (->> (map (fn [[s l]]
                (->> mapping-fns
                     (reduce (fn [intervals mapping-fn]
                               (reduce (fn [intervals interval]
                                         (concat intervals (mapping-fn interval)))
                                       []
                                       intervals))
                             [[s (+ s l)]])
                     (map first)
                     (apply min)))
              seed-partitions)
         (apply min))))


(comment
  (solve-a input)
  ;; 173706076

  (time (solve-b input))
  ;; 11611182
  ;;
  )

