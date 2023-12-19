(ns advent-of-code-2023.day-19
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day19.txt"))
(def test-input "px{a<2006:qkq,m>2090:A,rfg}\npv{a>1716:R,A}\nlnx{m>1548:A,A}\nrfg{s<537:gd,x>2440:R,A}\nqs{s>3448:A,lnx}\nqkq{x<1416:A,crn}\ncrn{x>2662:A,R}\nin{s<1351:px,qqz}\nqqz{s>2770:qs,m<1801:hdj,R}\ngd{a>3333:R,R}\nhdj{m>838:A,pv}\n\n{x=787,m=2655,a=1222,s=2876}\n{x=1679,m=44,a=2067,s=496}\n{x=2036,m=264,a=79,s=2244}\n{x=2461,m=1339,a=466,s=291}\n{x=2127,m=1623,a=2188,s=1013}")

(defn check-condition
  [v op num x m a s]
  (condp = v
    "x" ((eval (read-string op)) x (read-string num))
    "m" ((eval (read-string op)) m (read-string num))
    "a" ((eval (read-string op)) a (read-string num))
    "s" ((eval (read-string op)) s (read-string num))))

(defn get-fn
  ;(:test (fn []
  ;         (is= (get-fn "px{a<2006:qkq,m>2090:A,rfg}")
  ;              {:px (fn [fns x m a s]
  ;                      (cond
  ;                          (< a 2006) ((get fns :qkq) fns x m a s)
  ;                          (> m 2090) :A
  ;                          :else ((get fns :rfg) fns x m a s))
  ;                     )})))
  [line]
  (let [[_ id conditions] (re-find #"(\w+)\{(.*)\}" line)
        conditions (clojure.string/split conditions #",")]
    {(keyword id) (fn [fns x m a s]
                    (loop [[condition & conditions] conditions]
                      (if (empty? conditions)
                        (if (contains? #{:A :R} (keyword condition))
                          (keyword condition)
                          ((get fns (keyword condition)) fns x m a s))
                        (let [[_ v op num res] (re-find #"(\w)(<|>)(\d+):(\w+)" condition)]
                          (if (check-condition v op num x m a s)
                            (if (contains? #{:A :R} (keyword res))
                              (keyword res)
                              ((get fns (keyword res)) fns x m a s))
                            (recur conditions))))))}))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 19114))}
  [input]
  (let [[conditions ratings] (clojure.string/split input #"\n\n")
        fns (reduce (fn [a c]
                      (merge a (get-fn c)))
                    {}
                    (clojure.string/split-lines conditions))]
    (reduce (fn [acc r]
              (let [[x m a s] (map read-string (re-seq #"\d+" r))
                    res ((get fns :in) fns x m a s)]
                (if (= res :A)
                  (+ acc x m a s)
                  acc)))
            0
            (clojure.string/split-lines ratings))))

(defn solve-b
  {:test (fn []
           (is= (solve-b test-input) 167409079868000))}
  [input]
  952408144115)

(comment
  (time (solve-a input))
  ;; 367602
  ;; "Elapsed time: 18.701917 msecs"

  (time (solve-b input))
  ;;
  ;;
  )
