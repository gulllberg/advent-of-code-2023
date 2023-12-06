(ns advent-of-code-2023.day-06
  (:require [ysera.test :refer [is= is is-not]]
            [clojure.math]))

(def input (slurp "src/advent_of_code_2023/inputs/day06.txt"))
(def test-input "Time:      7  15   30\nDistance:  9  40  200")

(defn get-interval-length
  [race-time race-record]
  (clojure.math/sqrt (- (/ (clojure.math/pow race-time 2) 4) race-record 1)))

(defn solve-one-race
  [race-time race-record]
  (let [interval-length (get-interval-length race-time race-record)]
    (inc (int (- (clojure.math/floor (+ (/ race-time 2) interval-length))
                 (clojure.math/ceil (- (/ race-time 2) interval-length)))))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 288))}
  [input]
  (let [rows (clojure.string/split-lines input)
        race-times (map read-string (re-seq #"\d+" (first rows)))
        race-records (map read-string (re-seq #"\d+" (second rows)))]
    (reduce (fn [a [race-time race-record]]
              (* a (solve-one-race race-time race-record)))
            1
            (seq (zipmap race-times race-records)))))



(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 46))}
  [input]
  (let [rows (clojure.string/split-lines input)
        race-time (->> (re-seq #"\d+" (first rows))
                        (apply str)
                        (read-string))
        race-record (->> (re-seq #"\d+" (second rows))
                          (apply str)
                          (read-string))]
    (solve-one-race race-time race-record)))


(comment
  (time (solve-a input))
  ; "Elapsed time: 0.46783 msecs"
  ; => 252000

  (time (solve-b input))
  ; "Elapsed time: 0.423096 msecs"
  ; => 36992486
  )


;; Quick definition


;t_lopp, d_rekord
;
;d = v * t_färdas
;v = t_acc
;t_färdas = t_lopp - t_acc
;
;
;
;d = t_acc * (t_lopp - t_acc)
;
;d > d_rekord
;d >= d_rekord + 1
;
;-t_acc^2 + t_lopp*t_acc = d_rekord + 1
;
;t_acc^2 - t_acc*t_lopp + d_rekord + 1 = 0
;
;t_acc = t_lopp/2 +/- sqrt (t_lopp^2/4  - d_rekord - 1 )
;
;2 *  sqrt (t_lopp^2/4  - d_rekord - 1 ) (kanske x+ 1)
