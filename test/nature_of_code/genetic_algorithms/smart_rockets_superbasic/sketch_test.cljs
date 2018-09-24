(ns nature-of-code.genetic-algorithms.smart-rockets-superbasic.sketch-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [nature-of-code.genetic-algorithms.smart-rockets-superbasic.sketch :as sketch]))

(defn approx= ; TODO -> test utils
  "@see clojure.algo.generic.math-functions"
  [x y tolerance] 
    (< (Math/abs (- x y)) tolerance))

(defn seq-approx= ; TODO -> test utils 
  "Paarweiser Vergleich von zwei Zahlen-Sequenzen"
  [seq1 seq2 tolerance]
    (every? identity (map #(approx= %1 %2 tolerance) seq1 seq2)))

(deftest test-forces ; forces = genes
  (testing "random-forces"
    (let [forces (sketch/random-forces 8)]
      (is (= (count forces) 8))))
  (testing "crossover"
    (let [forces1 [1 1 1 1 1 1 1 1]
          forces2 [2 2 2 2 2 2 2 2]
          result-0 (sketch/crossover forces1 forces2 0)
          result-1 (sketch/crossover forces1 forces2 1)
          result-4 (sketch/crossover forces1 forces2 4)
          result-7 (sketch/crossover forces1 forces2 7)
          result-8 (sketch/crossover forces1 forces2 8)]
      (is (= (count result-4) (count forces1) (count forces2)))
      (is (= result-0 [2 2 2 2 2 2 2 2]))
      (is (= result-1 [1 2 2 2 2 2 2 2]))
      (is (= result-4 [1 1 1 1 2 2 2 2])) ; check split-at idx
      (is (= result-7 [1 1 1 1 1 1 1 2]))
      (is (= result-8 [1 1 1 1 1 1 1 1])))
    (let [forces1 [1 2 3 4 5 6 7 8]
          forces2 [8 7 6 5 4 3 2 1]
          result-4 (sketch/crossover forces1 forces2 4)]
      (is (= (count result-4) (count forces1) (count forces2)))
      (is (= result-4 [1 2 3 4 4 3 2 1])))) ; bleibt Reihenfolge erhalten ?
  (testing "mutate"
    (with-redefs [sketch/random-force (fn [_] 2)]
      (let [forces [1 2 3 4 5 6 7 8]
            result-0 (sketch/mutate forces 0.0)
            result-1 (sketch/mutate forces 1.0)
            result-1-1 (sketch/mutate result-1 0.5)]
        (is (= result-0 [1 2 3 4 5 6 7 8]))
        (is (= result-1 [2 2 2 2 2 2 2 2]))))))

(deftest test-rocket
  (testing "next-motion-state"
    (let [rocket {:id "r1" :mass 4 :location [1 2] :velocity [0.1 0.2] :forces [[0.04 0.08] [-0.04 -0.08]] :force-index 0,
                   :rocket-r 4 :fitness 0 :min-d 9007199254740991 :hit-target false}
          result-1 (sketch/next-motion-state rocket)
          result-2 (sketch/next-motion-state result-1)]
      (print (str result-1))
      (print (str result-2))
      (is (seq-approx= (:location result-1) [1.1 2.2] 1e-4))
      (is (seq-approx= (:location result-2) [1.21 2.42] 1e-4)))))
