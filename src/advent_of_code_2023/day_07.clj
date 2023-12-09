(ns advent-of-code-2023.day-07
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day07.txt"))
(def test-input "32T3K 765\nT55J5 684\nKK677 28\nKTJJT 220\nQQQJA 483")

(defn get-score
  {:test (fn []
           (is= (get-score "23456") 0)
           (is= (get-score "32T3K") 1)
           (is= (get-score "KK677") 2)
           (is= (get-score "KTJJJ") 3)
           (is= (get-score "23332") 4)
           (is= (get-score "T5555") 5)
           (is= (get-score "QQQQQ") 6))}
  [hand]
  (let [l (->> (frequencies hand)
               (vals)
               (sort)
               (reverse))]
    (cond (= (first l) 5) 6
          (= (first l) 4) 5
          (= [(first l) (second l)] [3 2]) 4
          (= (first l) 3) 3
          (= [(first l) (second l)] [2 2]) 2
          (= (first l) 2) 1
          :else 0)))

(def card->value (zipmap [\2 \3 \4 \5 \6 \7 \8 \9 \T \J \Q \K \A] (range 2 15)))

(defn get-compare-fn
  {:test (fn []
           (is (neg? ((get-compare-fn get-score card->value) "32T3K" "T55J5")))
           (is (pos? ((get-compare-fn get-score card->value) "KK677" "KTJJT"))))}
  [get-score card->value]
  (fn [hand1 hand2]
    (let [score1 (get-score hand1)
          score2 (get-score hand2)]
      (if (not= score1 score2)
        (- score1 score2)
        (loop [[h1 & t1] hand1
               [h2 & t2] hand2]
          (let [v1 (card->value h1)
                v2 (card->value h2)]
            (if (= v1 v2)
              (recur t1 t2)
              (- v1 v2))))))))

(defn solve-a
  {:test (fn [] (is= (solve-a test-input) 6440))}
  [input]
  (->> (clojure.string/split-lines input)
       (map (fn [r] (clojure.string/split r #" ")))
       (map (fn [[hand bid-as-sting]] {:hand hand :bid (read-string bid-as-sting)}))
       (sort-by :hand (get-compare-fn get-score card->value))
       (map-indexed (fn [i {bid :bid}] (* (inc i) bid)))
       (reduce +)))

(defn get-score-2
  {:test (fn []
           (is= (get-score-2 "KTJJT") 5)
           (is= (get-score-2 "T55J5") 5)
           (is= (get-score-2 "QQQJA") 5))}
  [hand]
  (if (= hand "JJJJJ")
    6
    (let [f (frequencies hand)
          number-of-jokers (get f \J 0)
          l (as-> (dissoc f \J) $
                  (vals $)
                  (sort $)
                  (reverse $)
                  (into [] $)
                  (update $ 0 + number-of-jokers))]
      (cond (= (first l) 5) 6
            (= (first l) 4) 5
            (= [(first l) (second l)] [3 2]) 4
            (= (first l) 3) 3
            (= [(first l) (second l)] [2 2]) 2
            (= (first l) 2) 1
            :else 0))))

(def card->value-2 (zipmap [\J \2 \3 \4 \5 \6 \7 \8 \9 \T \Q \K \A] (range 2 15)))

(defn solve-b
  {:test (fn [] (is= (solve-b test-input) 5905))}
  [input]
  (->> (clojure.string/split-lines input)
       (map (fn [r] (clojure.string/split r #" ")))
       (map (fn [[hand bid-as-sting]] {:hand hand :bid (read-string bid-as-sting)}))
       (sort-by :hand (get-compare-fn get-score-2 card->value-2))
       (map-indexed (fn [i {bid :bid}] (* (inc i) bid)))
       (reduce +)))

(comment
  (time (solve-a input))
  ;; 248113761
  ;; "Elapsed time: 19.037125 msecs"

  (time (solve-b input))
  ;; 246285222
  ;; "Elapsed time: 21.611375 msecs"
  )

