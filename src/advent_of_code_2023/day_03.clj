(ns advent-of-code-2023.day-03
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day03.txt"))
(def test-input "467..114..\n...*......\n..35..633.\n......#...\n617*......\n.....+.58.\n..592.....\n......755.\n...$.*....\n.664.598..")

(defn get-symbols-in-row
  {:test (fn []
           (is= (get-symbols-in-row (clojure.string/split "..12." #"")) #{})
           (is= (get-symbols-in-row (clojure.string/split "..1$2.#" #"")) #{3 6}))}
  [row]
  (reduce (fn [a i]
            (if (re-find #"[^\d\.]" (nth row i))
              (conj a i)
              a))
          #{}
          (range (count row))))

(defn parse-number
  {:test (fn []
           (is= (parse-number []) nil)
           (is= (parse-number ["1" "2" "3"]) 123))}
  [digits]
  (when-not (empty? digits)
    (read-string (apply str digits))))

(defn complete-number
  {:test (fn []
           (is= (complete-number ["6" "6" "4"] [1 2 3] #{3 5} #{} #{}) 664))}
  [digits digits-indexes symbols-prev-row symbols-this-row symbols-next-row]
  (if (zero? (count digits))
    0
    (let [adjacent-above-or-under (reduce (fn [a i]
                                            (if (or (contains? symbols-prev-row i)
                                                    (contains? symbols-next-row i))
                                              (reduced true)
                                              a))
                                          false
                                          (range (first digits-indexes) (inc (last digits-indexes))))
          adjacent (or adjacent-above-or-under
                       (contains? symbols-prev-row (dec (first digits-indexes)))
                       (contains? symbols-this-row (dec (first digits-indexes)))
                       (contains? symbols-next-row (dec (first digits-indexes)))
                       (contains? symbols-prev-row (inc (last digits-indexes)))
                       (contains? symbols-this-row (inc (last digits-indexes)))
                       (contains? symbols-next-row (inc (last digits-indexes))))]
      (if adjacent
        (parse-number digits)
        0))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 4361))}
  [input]
  (let [rows (map (fn [line]
                    (clojure.string/split line #""))
                  (clojure.string/split-lines input))]
    (loop [symbols-prev-row #{}
           symbols-this-row (get-symbols-in-row (first rows))
           symbols-next-row (get-symbols-in-row (second rows))
           i 0
           sum 0]
      (let [row (nth rows i)
            sum (loop [j 0
                       sum sum
                       digits []
                       digits-indexes []]
                  (let [char (if (< j (count row)) (nth row j) nil)]
                    (cond
                      ;; Finished row
                      (>= j (count row))
                      (+ sum (complete-number digits digits-indexes symbols-prev-row symbols-this-row symbols-next-row))

                      ;; Got a digit
                      (re-find #"\d" char)
                      (recur (inc j) sum (conj digits char) (conj digits-indexes j))

                      ;; Not a digit, either completed a number one or no number at all
                      :else
                      (recur (inc j)
                             (+ sum (complete-number digits digits-indexes symbols-prev-row symbols-this-row symbols-next-row))
                             []
                             []))))]
        (if
          ;; Was last row
          (= i (dec (count rows)))
          sum

          (recur symbols-this-row
                 symbols-next-row
                 (if (= i (- (count rows) 2))
                   #{}
                   (get-symbols-in-row (nth rows (+ i 2))))
                 (inc i)
                 sum))))))

(defn get-numbers-in-row
  {:test (fn []
           (is= (get-numbers-in-row (clojure.string/split "..12." #"")) {2 12
                                                                         3 12})
           (is= (get-numbers-in-row (clojure.string/split "..1$2.#" #"")) {2 1
                                                                           4 2}))}
  [row]
  (let [[res digits indexes] (reduce (fn [[a digits indexes] i]
                                       (let [digit (re-find #"\d" (nth row i))]
                                         (if digit
                                           [a (conj digits digit) (conj indexes i)]
                                           (let [number (parse-number digits)]
                                             (if-not number
                                               [a [] []]
                                               [(reduce (fn [a i]
                                                          (assoc a i number))
                                                        a
                                                        indexes)
                                                [] []])))))
                                     [{} [] []]
                                     (range (count row)))
        number (parse-number digits)]
    (if-not number
      res
      (reduce (fn [a i]
                (assoc a i number))
              res
              indexes))))

(defn complete-gear
  [j numbers-prev-row numbers-this-row numbers-next-row]
  (let [number-left (get numbers-this-row (dec j))
        number-right (get numbers-this-row (inc j))
        number-up (get numbers-prev-row j)
        number-down (get numbers-next-row j)
        number-up-left (when-not number-up (get numbers-prev-row (dec j)))
        number-up-right (when-not number-up (get numbers-prev-row (inc j)))
        number-down-left (when-not number-down (get numbers-next-row (dec j)))
        number-down-right (when-not number-down (get numbers-next-row (inc j)))
        gear-numbers (filter identity [number-left number-right number-up number-down number-up-left number-up-right number-down-left number-down-right])]
    (if (= (count gear-numbers) 2)
      (* (first gear-numbers) (second gear-numbers))
      0)))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 467835))}
  [input]
  (let [rows (map (fn [line]
                    (clojure.string/split line #""))
                  (clojure.string/split-lines input))]
    (loop [numbers-prev-row {}
           numbers-this-row (get-numbers-in-row (first rows))
           numbers-next-row (get-numbers-in-row (second rows))
           i 0
           sum 0]
      (let [row (nth rows i)
            sum (loop [j 0
                       sum sum]
                  (if
                    ;; Finished row
                    (>= j (count row))
                    sum

                    (let [char (if (< j (count row)) (nth row j) nil)]
                      (if
                        ;; Got a gear
                        (re-find #"\*" char)
                        (recur (inc j) (+ sum (complete-gear j numbers-prev-row numbers-this-row numbers-next-row)))

                        ;; Not a gear
                        (recur (inc j) sum)))))]
        (if
          ;; Was last row
          (= i (dec (count rows)))
          sum

          (recur numbers-this-row
                 numbers-next-row
                 (if (= i (- (count rows) 2))
                   {}
                   (get-numbers-in-row (nth rows (+ i 2))))
                 (inc i)
                 sum))))))

(comment
  (solve-a input)
  ;; 535078

  (solve-b input)
  ;; 75312571
  )

