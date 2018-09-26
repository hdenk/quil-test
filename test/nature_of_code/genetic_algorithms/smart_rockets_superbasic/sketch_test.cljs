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
  (testing "gen-forces"
    (let [forces (sketch/gen-forces 8)]
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
    (let [rocket {:mass 4 :location [1 2] :velocity [0.1 0.2] :forces [[0.04 0.08] [-0.04 -0.08]] :force-index 0}
          result-1 (sketch/next-motion-state rocket)
          result-2 (sketch/next-motion-state result-1)]
      (is (seq-approx= (:location result-1) [1.1 2.2] 1e-4))
      (is (seq-approx= (:velocity result-1) [0.11 0.22] 1e-4))
      (is (seq-approx= (:location result-2) [1.21 2.42] 1e-4))
      (is (seq-approx= (:velocity result-2) [0.1 0.2] 1e-4))))
  (testing "check-hit-target"
    (with-redefs [sketch/config {:target-r 100}] ; TODO dep config is bad
      (let [rocket {:location [200 200] :hit-target false}
            target-0 [100 100]
            result-0 (sketch/check-hit-target rocket target-0)
            target-1 [200 200]
            result-1 (sketch/check-hit-target rocket target-1)
            target-2 [200 100]
            result-2 (sketch/check-hit-target rocket target-2)
            target-3 [100 200]
            result-3 (sketch/check-hit-target rocket target-3)
            target-4 [200 101]
            result-4 (sketch/check-hit-target rocket target-4)
            target-5 [101 200]
            result-5 (sketch/check-hit-target rocket target-5)]
        (is (= (:hit-target result-0) false))
        (is (= (:hit-target result-1) true))
        (is (= (:hit-target result-2) false))
        (is (= (:hit-target result-3) false))
        (is (= (:hit-target result-4) true)))))
  (testing "fitness"
    (with-redefs [sketch/config {:lifetime 100}] ; TODO dep config is bad
      (let [rocket-1 {:force-index 100}
            result-1 (sketch/fitness rocket-1)
            rocket-2 {:force-index 90}
            result-2 (sketch/fitness rocket-2)
            rocket-3 {:force-index 0}
            result-3 (sketch/fitness rocket-3)]
        (is (= (:fitness result-1) 0))
        (is (= (:fitness result-2) (* 10 10)))
        (is (= (:fitness result-3) (* 100 100)))))))
