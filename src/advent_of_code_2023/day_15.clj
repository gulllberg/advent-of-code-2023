(ns advent-of-code-2023.day-15
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day15.txt"))
(def test-input "rn=1,cm-,qp=3,cm=2,qp-,pc=4,ot=9,ab=5,pc-,pc=6,ot=7")

(defn holiday-ascii-string-helper
  [string]
  (reduce (fn [a c]
            (-> c
                  (int)
                  (+ a)
                  (* 17)
                  (mod 256)))
          0
          string))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 1320))}
  [input]
  (->> (clojure.string/split (clojure.string/trim-newline input) #",")
       (map holiday-ascii-string-helper)
       (reduce +)))

(defn indices [pred coll]
  (keep-indexed #(when (pred %2) %1) coll))

(defn holiday-ascii-string-helper-manual-arrangement-procedure
  [state string]
  (let [label (re-find #"[a-z]+" string)
        label-hash (holiday-ascii-string-helper label)
        operation (re-find #"=|-" string)
        focal-length (if (re-find #"\d" string) (read-string (re-find #"\d" string)) nil)]
    (update state label-hash (fn [v]
                               (if (nil? v)
                                 (if (= operation "=")
                                   [[label focal-length]]
                                   [])
                                 (let [old-lens-index (first (indices (fn [[l _]]
                                                                        (= l label))
                                                                      v))]
                                   (if old-lens-index
                                     (if (= operation "=")
                                       (assoc v old-lens-index [label focal-length])
                                       (->> (assoc v old-lens-index nil)
                                            (remove nil?)
                                            (into [])))
                                     (if (= operation "=")
                                       (conj v [label focal-length])
                                       v))))))))

(defn calculate-focusing-power
  [state]
  (reduce-kv (fn [a label-hash box]
               (reduce-kv (fn [a i [_ focal-length]]
                            (+ a (* (inc label-hash) (inc i) focal-length)))
                          a
                          box))
             0
             state))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 145))}
  [input]
  (->> (clojure.string/split (clojure.string/trim-newline input) #",")
       (reduce holiday-ascii-string-helper-manual-arrangement-procedure {})
       (calculate-focusing-power)))

(comment
  (time (solve-a input))
  ;; 514281
  ;; "Elapsed time: 2.742333 msecs"

  (time (solve-b input))
  ;; 244199
  ;; "Elapsed time: 6.694208 msecs"

  )
