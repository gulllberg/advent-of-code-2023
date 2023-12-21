(ns advent-of-code-2023.day-20
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code_2023/inputs/day20.txt"))
(def test-input "broadcaster -> a, b, c\n%a -> b\n%b -> c\n%c -> inv\n&inv -> a")
(def test-input-2 "broadcaster -> a\n%a -> inv, con\n&inv -> b\n%b -> con\n&con -> output")

(defn create-state
  [input]
  (let [state-without-conjunction-inputs (reduce (fn [a line]
                                                   (let [[module targets] (clojure.string/split line #" -> ")
                                                         targets (map keyword (clojure.string/split targets #", "))]
                                                     (cond
                                                       (= module "broadcaster")
                                                       (assoc a :broadcaster {:targets targets
                                                                              :type    :broadcaster})

                                                       (re-find #"%[a-z]+" module)
                                                       (let [[_ module-name] (re-find #"%([a-z]+)" module)]
                                                         (assoc a (keyword module-name) {:targets targets
                                                                                         :type    :flip-flop
                                                                                         :state   :off}))
                                                       :else
                                                       (let [[_ module-name] (re-find #"&([a-z]+)" module)]
                                                         (assoc a (keyword module-name) {:targets targets
                                                                                         :type    :conjunction
                                                                                         :inputs  {}})))))
                                                 {}
                                                 (clojure.string/split-lines input))]
    (reduce-kv (fn [a k v]
                 (reduce (fn [a t]
                           (if (= :conjunction (get-in a [t :type]))
                             (update-in a [t :inputs] (fn [i]
                                                        (assoc i k :low)))
                             a))
                         a
                         (:targets v)))
               state-without-conjunction-inputs
               state-without-conjunction-inputs)))

(defn handle-pulse
  [state [from to type]]
  (let [destination-type (get-in state [to :type])
        targets (get-in state [to :targets])]
    (condp = destination-type
      :broadcaster [state (map (fn [t]
                                 [to t type])
                               targets)]

      :flip-flop (if (= type :high)
                   [state []]
                   (let [old-state (get-in state [to :state])]
                     (if (= old-state :off)
                       [(assoc-in state [to :state] :on) (map (fn [t]
                                                                [to t :high])
                                                              targets)]
                       [(assoc-in state [to :state] :off) (map (fn [t]
                                                                 [to t :low])
                                                               targets)])))

      (let [state (assoc-in state [to :inputs from] type)]
        (if (->> (get-in state [to :inputs])
                 (vals)
                 (remove (fn [s] (= s :high)))
                 (empty?))
          [state (map (fn [t]
                        [to t :low])
                      targets)]
          [state (map (fn [t]
                        [to t :high])
                      targets)])))))

(defn push-button
  {:test (fn []
           (is= (push-button (create-state test-input))
                [(create-state test-input) 4 8]))}
  [state]
  (loop [state state
         pulses [[nil :broadcaster :low]]
         num-high 0
         num-low 0]
    (if (empty? pulses)
      [state num-high num-low]
      (let [[pulse & rest-pulses] pulses
            is-high-pulse (= (last pulse) :high)
            [new-state new-pulses] (handle-pulse state pulse)]
        (recur new-state (concat rest-pulses new-pulses) (if is-high-pulse (inc num-high) num-high) (if is-high-pulse num-low (inc num-low)))))))

(defn solve-a
  {:test (fn []
           (is= (solve-a test-input) 32000000)
           (is= (solve-a test-input-2) 11687500))}
  [input]
  (let [state (create-state input)]
    (let [[_ num-high num-low] (reduce (fn [[state num-high num-low] _]
                                         (let [[state n-h n-l] (push-button state)]
                                           [state (+ num-high n-h) (+ num-low n-l)]))
                                       [state 0 0]
                                       (range 1000))]
      (* num-high num-low))))

(defn push-button-b
  [state num-pushed cycles]
  (loop [state state
         pulses [[nil :broadcaster :low]]
         cycles cycles]
    (if (empty? pulses)
      [state cycles]
      (let [[pulse & rest-pulses] pulses
            [new-state new-pulses] (handle-pulse state pulse)]
        (recur new-state
               (concat rest-pulses new-pulses)
               (if (and (= (last pulse) :high)
                        (nil? (get cycles (first pulse) 0)))
                 (assoc cycles (first pulse) num-pushed)
                 cycles))))))

(defn solve-b
  [input]
  (let [state (create-state input)]
    (loop [state state
           num-pushed 1
           ;; Not a general solution.
           ;; Based on some observations of the input
           cycles {:lt nil
                   :qh nil
                   :bq nil
                   :vz nil}]
      (let [[state cycles] (push-button-b state num-pushed cycles)]
        (if (->> cycles
                 (vals)
                 (remove nil?)
                 (count)
                 (= 4))
          (->> cycles
               (vals)
               (reduce *))
          (recur state (inc num-pushed) cycles))))))

(comment
  (time (solve-a input))
  ;; 737679780
  ;; "Elapsed time: 50.701541 msecs"

  (time (solve-b input))
  ;; 227411378431763
  ;; "Elapsed time: 178.72825 msecs"
  )
