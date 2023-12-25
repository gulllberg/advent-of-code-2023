(ns advent-of-code-2023.day-24
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day24.txt"))
(def test-input "19, 13, 30 @ -2,  1, -2\n18, 19, 22 @ -1, -1, -2\n20, 25, 34 @ -2, -2, -4\n12, 31, 28 @ -1, -2, -1\n20, 19, 15 @  1, -5, -3")

(defn parse-line
  [line]
  (map read-string (re-seq #"[-\d]+" line)))

(defn paths-intersect?
  [h1 h2 min-xy max-xy]
  (let [[x1 y1 _ vx1 vy1 _] h1
        [x2 y2 _ vx2 vy2 _] h2]
    ;; None of v = 0 in puzzle, so no need to check for that
    (if (= (/ vx1 vx2) (/ vy1 vy2))
      ;; Parallel
      false
      (let [t (/ (- (/ (- x2 x1) vx2)
                    (/ (- y2 y1) vy2))
                 (- (/ vx1 vx2)
                    (/ vy1 vy2)))
            s (/ (+ (* t vx1) x1 (- x2))
                 vx2)]
        (if (or (< t 0)
                (< s 0))
          ;; Paths cross in negative time
          false
          (let [xi (+ x1 (* t vx1))
                yi (+ y1 (* t vy1))]
            (and (<= min-xy xi max-xy)
                 (<= min-xy yi max-xy))))))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input 7 27) 2))}
  [input min-xy max-xy]
  (let [hails (->> input
                   (clojure.string/split-lines)
                   (map parse-line))]
    (reduce (fn [a i]
              (let [this-hail (nth hails i)]
                (reduce (fn [a other-hail]
                          (if (paths-intersect? this-hail other-hail min-xy max-xy)
                            (inc a)
                            a))
                        a
                        (drop (inc i) hails))))
            0
            (range (count hails)))))

(defn intersects-parallel-in-xy?
  [h1 h2]
  (let [[_ y1 z1 _ _ vz1] h1
        [_ y2 z2 _ _ vz2] h2]
    (if-not (= y1 y2)
      false
      (if (= vz1 vz2)
        ;; Parallell in z
        (= z1 z2)
        (let [t (/ (- z2 z1) (- vz1 vz2))]
          (if (< t 0)
            ;; Negative time (in z)
            false
            true))))))

(defn intersects-parallel-x?
  [h1 h2]
  (let [[x1 y1 z1 _ vy1 vz1] h1
        [x2 y2 z2 _ vy2 vz2] h2]
    (if-not (= x1 x2)
      false
      (if (= vy1 vy2)
        ;; Parallell in y
        (intersects-parallel-in-xy? h1 h2)
        (let [t (/ (- y2 y1) (- vy1 vy2))]
          (if (< t 0)
            ;; Negative time (in y)
            false
            (let [zi1 (+ z1 (* t vz1))
                  zi2 (+ z2 (* t vz2))]
              ;; intersects in z?
              (= zi1 zi2))))))))

(defn intersects?
  [h1 h2]
  (let [[x1 y1 z1 vx1 vy1 vz1] h1
        [x2 y2 z2 vx2 vy2 vz2] h2]
    (if (= vx1 vx2)
      ;; Parallel in x
      (intersects-parallel-x? h1 h2)
      (let [t (/ (- x2 x1) (- vx1 vx2))]
        (if (< t 0)
          ;; Negative time (in x)
          false
          (let [yi1 (+ y1 (* t vy1))
                yi2 (+ y2 (* t vy2))]
            (if-not (= yi1 yi2)
              ;; Not intersect in y
              false
              (let [zi1 (+ z1 (* t vz1))
                    zi2 (+ z2 (* t vz2))]
                ;; intersects in z?
                (= zi1 zi2)))))))))

;; Based on algebra (variable substitution)
;; Using the fact that intersection happens at time t
;; And then x1+vx1*t=x2+vx2*t, etc
(defn get-y1
  [vx1 vy1 x2 y2 vx2 vy2 x3 y3 vx3 vy3]
  (/ (+ x2
        (- x3)
        (* y2 (/ (- vx2 vx1)
                 (- vy1 vy2)))
        (- (* y3 (/ (- vx3 vx1)
                    (- vy1 vy3)))))
     (- (/ (- vx2 vx1)
           (- vy1 vy2))
        (/ (- vx3 vx1)
           (- vy1 vy3)))))

(defn get-t
  [y1 vy1 y2 vy2]
  (/ (- y2 y1)
     (- vy1 vy2)))

(defn get-x1
  [t vx1 x2 vx2]
  (- x2 (* t (- vx1 vx2))))

(defn get-vz1
  [y1 vy1 y2 z2 vy2 vz2 y3 z3 vy3 vz3]
  (/ (+ z3
        (- z2)
        (* vz3 (/ (- y3 y1)
                  (- vy1 vy3)))
        (- (* vz2 (/ (- y2 y1)
                     (- vy1 vy2)))))
     ;; Note: This could maybe have been division by zero, but was not on given data
     (- (/ (- y3 y1)
           (- vy1 vy3))
        (/ (- y2 y1)
           (- vy1 vy2)))))

(defn get-z1
  [t vz1 z2 vz2]
  (- z2 (* t (- vz1 vz2))))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 47))}
  [input]
  (let [hails (->> input
                   (clojure.string/split-lines)
                   (map parse-line))
        [x y z vx vy vz] (->> (for [vx1 (range -1000 1001)
                                    vy1 (range -1000 1001)]
                                [vx1 vy1])
                              (reduce (fn [_ [vx1 vy1]]
                                        (let [h2 (first (remove (fn [h]
                                                                  (let [[_ _ _ vx vy _] h]
                                                                    (or (= vx1 vx)
                                                                        (= vy1 vy))))
                                                                hails))
                                              [x2 y2 z2 vx2 vy2 vz2] h2
                                              h3 (first (remove (fn [h]
                                                                  (let [[_ _ _ vx3 vy3 _] h]
                                                                    (or (= h h2)
                                                                        (= vx1 vx3)
                                                                        (= vy1 vy3)
                                                                        (= 0 (- (/ (- vx2 vx1)
                                                                                   (- vy1 vy2))
                                                                                (/ (- vx3 vx1)
                                                                                   (- vy1 vy3)))))))
                                                                hails))
                                              [x3 y3 z3 vx3 vy3 vz3] h3]
                                          (let [y1 (get-y1 vx1 vy1 x2 y2 vx2 vy2 x3 y3 vx3 vy3)
                                                t (get-t y1 vy1 y2 vy2)]
                                            (if (< t 0)
                                              nil
                                              (let [x1 (get-x1 t vx1 x2 vx2)
                                                    vz1 (get-vz1 y1 vy1 y2 z2 vy2 vz2 y3 z3 vy3 vz3)
                                                    z1 (get-z1 t vz1 z2 vz2)
                                                    h1 [x1 y1 z1 vx1 vy1 vz1]]
                                                (if (reduce (fn [_ h]
                                                              (if (intersects? h1 h)
                                                                true
                                                                (reduced false)))
                                                            nil
                                                            hails)
                                                  (reduced h1)
                                                  nil))))))
                                      nil))]
    (println "answer" x y z vx vy vz)
    (when (and x y z)
      (+ x y z))))

(comment
  (time (solve-a input 200000000000000 400000000000000))
  ;; 11246
  ;; "Elapsed time: 160.9725 msecs"

  (time (solve-b input))
  ;; 716599937560103
  ;; "Elapsed time: 117888.086292 msecs"
  )
