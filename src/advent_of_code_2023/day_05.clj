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

(defn do-seed-partition
  [seed-partition mapping-fns]
  (let [start (first seed-partition)
        length (second seed-partition)
        end (+ start length)]
    (loop [seed start
           min-location ##Inf]
      (if (>= seed end)
        min-location
        (recur (inc seed) (min min-location (reduce (fn [a mapping-fn]
                                                      (mapping-fn a))
                                                    seed
                                                    mapping-fns)))))))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 46))}
  [input]
  (let [almanac (clojure.string/split input #"\n\n")
        seed-partitions (->> (re-seq #"\d+" (first almanac))
                             (map read-string)
                             (partition 2))
        mapping-fns (map combine-mapping-fns (rest almanac))]
    (println "number of seed partitions")
    (println (count seed-partitions))
    (reduce (fn [min-location seed-partition]
              (println "starting seed partition")
              (println seed-partition)
              (min min-location (do-seed-partition seed-partition mapping-fns)))
            ##Inf
            seed-partitions)))


(comment
  (solve-a input)
  ;; 173706076

  (time (solve-b input))
  ;; 11611182
  ;; "Elapsed time: 2.7138193503851E7 msecs"
  ;; Oops...
  )

