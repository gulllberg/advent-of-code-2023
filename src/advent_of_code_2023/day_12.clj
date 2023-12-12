(ns advent-of-code-2023.day-12
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day12.txt"))
(def test-input "???.### 1,1,3\n.??..??...?##. 1,1,3\n?#?#?#?#?#?#?#? 1,3,1,6\n????.#...#... 4,1,1\n????.######..#####. 1,6,5\n?###???????? 3,2,1")


(def solve-spring-groups
  (memoize (fn [spring-groups damaged-springs]
             (let [spring-group (first spring-groups)
                   number-of-damaged (first damaged-springs)]
               (cond
                 ;; Finished
                 (and (empty? spring-groups) (empty? damaged-springs))
                 1

                 ;; Impossible
                 (empty? spring-groups)
                 0

                 ;; Finished
                 (and (empty? damaged-springs)
                      (nil? (clojure.string/index-of (apply str spring-groups) "#")))
                 1

                 ;; Impossible
                 (empty? damaged-springs)
                 0

                 ;; Only ?, but too short, all must be functional .
                 (and (> number-of-damaged (count spring-group))
                      (nil? (clojure.string/index-of spring-group "#")))
                 (solve-spring-groups (rest spring-groups) damaged-springs)

                 ;; Too short, impossible
                 (> number-of-damaged (count spring-group))
                 0

                 ;; Exact length and only ?, can be solution
                 (and (= number-of-damaged (count spring-group))
                      (nil? (clojure.string/index-of spring-group "#")))
                 (+ (solve-spring-groups (rest spring-groups) damaged-springs)
                    (solve-spring-groups (rest spring-groups) (rest damaged-springs)))

                 ;; Exact length, must be solution
                 (= number-of-damaged (count spring-group))
                 (solve-spring-groups (rest spring-groups) (rest damaged-springs))

                 ;; No ? but too long, impossible
                 (nil? (clojure.string/index-of spring-group "?"))
                 0

                 ;; We must have ?, replace it with both cases and continue
                 :else
                 (let [split-group (clojure.string/split (clojure.string/replace-first spring-group "?" ".") #"\.")]
                   (+ (solve-spring-groups (concat (list (clojure.string/replace-first spring-group "?" "#")) (rest spring-groups)) damaged-springs)
                      (solve-spring-groups (concat split-group (rest spring-groups)) damaged-springs))))))))

(defn test-solve-spring-groups
  {:test (fn []
           (is= (test-solve-spring-groups (list "???" "###") (list 1 1 3)) 1)
           (is= (test-solve-spring-groups (list "??" "??" "?##") (list 1 1 3)) 4)
           (is= (test-solve-spring-groups (list "?#?#?#?#?#?#?#?") (list 1 3 1 6)) 1)
           (is= (test-solve-spring-groups (list "????" "#" "#") (list 4 1 1)) 1)
           (is= (test-solve-spring-groups (list "????" "######" "#####") (list 1 6 5)) 4)
           (is= (test-solve-spring-groups (list "?###????????") (list 3 2 1)) 10))}
  [spring-groups damaged-springs]
  (solve-spring-groups spring-groups damaged-springs))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 21))}
  [input]
  (->> input
       (clojure.string/split-lines)
       (map (fn [line]
              (let [spring-groups (re-seq #"[\?\#]+" line)
                    damaged-springs (map read-string (re-seq #"\d+" line))]
                (solve-spring-groups spring-groups damaged-springs))))
       (reduce +)))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 525152))}
  [input]
  (->> input
       (clojure.string/split-lines)
       (map (fn [line]
              (let [[p1 p2] (clojure.string/split line #" ")
                    spring-groups (re-seq #"[\?\#]+" (clojure.string/join "?" (repeat 5 p1)))
                    damaged-springs (map read-string (re-seq #"\d+" (clojure.string/join "," (repeat 5 p2))))]
                (solve-spring-groups spring-groups damaged-springs))))
       (reduce +)))

(comment
  (time (solve-a input))
  ;; 6958
  ;; "Elapsed time: 152.94038 msecs"

  ;; After memoize
  ;; "Elapsed time: 208.884497 msecs"
  ;; "Elapsed time: 23.608101 msecs"

  (time (solve-b input))
  ;; 6555315065024
  ;; "Elapsed time: 13558.893774 msecs"
  )
