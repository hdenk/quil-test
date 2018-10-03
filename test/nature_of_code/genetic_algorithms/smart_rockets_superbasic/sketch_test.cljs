(ns nature-of-code.genetic-algorithms.smart-rockets-superbasic.sketch-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [nature-of-code.testutils :refer [approx= seq-approx=]]
            [nature-of-code.genetic-algorithms.smart-rockets-superbasic.sketch :as sketch]))

(deftest test-forces ; forces = genes
  (testing "gen-forces"
    (let [forces (sketch/gen-forces 8)]
      (is (= (count forces) 8))))
  (testing "crossover-forces"
    (let [forces-1 [1 1 1 1 1 1 1 1]
          forces-2 [2 2 2 2 2 2 2 2]
          result-0 (sketch/crossover-forces forces-1 forces-2 0)
          result-1 (sketch/crossover-forces forces-1 forces-2 1)
          result-4 (sketch/crossover-forces forces-1 forces-2 4)
          result-7 (sketch/crossover-forces forces-1 forces-2 7)
          result-8 (sketch/crossover-forces forces-1 forces-2 8)]
      (is (= (count result-4) (count forces-1) (count forces-2)))
      (is (= result-0 [2 2 2 2 2 2 2 2]))
      (is (= result-1 [1 2 2 2 2 2 2 2]))
      (is (= result-4 [1 1 1 1 2 2 2 2])) ; check split-at idx
      (is (= result-7 [1 1 1 1 1 1 1 2]))
      (is (= result-8 [1 1 1 1 1 1 1 1])))
    (let [forces-1 [1 2 3 4 5 6 7 8]
          forces-2 [8 7 6 5 4 3 2 1]
          result-4 (sketch/crossover-forces forces-1 forces-2 4)]
      (is (= (count result-4) (count forces-1) (count forces-2)))
      (is (= result-4 [1 2 3 4 4 3 2 1])))) ; bleibt Reihenfolge erhalten ?
  (testing "mutate-forces"
    (with-redefs [sketch/random-force (fn [_] 2)]
      (let [forces [1 2 3 4 5 6 7 8]
            result-0 (sketch/mutate-forces forces 0.0)
            result-1 (sketch/mutate-forces forces 1.0)
            result-1-1 (sketch/mutate-forces result-1 0.5)]
        (is (= result-0 [1 2 3 4 5 6 7 8]))
        (is (= result-1 [2 2 2 2 2 2 2 2]))))))

(deftest test-rocket
  (testing "move-rocket"
    (let [rocket {:mass 4 :location [1 2] :velocity [0.1 0.2] :forces [[0.04 0.08] [-0.04 -0.08]] :force-index 0}
          result-1 (sketch/move-rocket rocket)
          result-2 (sketch/move-rocket result-1)]
      (is (seq-approx= (:location result-1) [1.1 2.2] 1e-4))
      (is (seq-approx= (:velocity result-1) [0.11 0.22] 1e-4))
      (is (seq-approx= (:location result-2) [1.21 2.42] 1e-4))
      (is (seq-approx= (:velocity result-2) [0.1 0.2] 1e-4))))
  (testing "check-rocket-hit-target"
    (let [rocket {:location [200 200] :hit-target false}
          target-0 {:location [100 100] :target-r 100}
          result-0 (sketch/check-rocket-hit-target rocket target-0)
          target-1 {:location [200 200] :target-r 100}
          result-1 (sketch/check-rocket-hit-target rocket target-1)
          target-2 {:location [200 100] :target-r 100}
          result-2 (sketch/check-rocket-hit-target rocket target-2)
          target-3 {:location [100 200] :target-r 100}
          result-3 (sketch/check-rocket-hit-target rocket target-3)
          target-4 {:location [200 101] :target-r 100}
          result-4 (sketch/check-rocket-hit-target rocket target-4)
          target-5 {:location [101 200] :target-r 100}
          result-5 (sketch/check-rocket-hit-target rocket target-5)]
      (is (= (:hit-target result-0) false))
      (is (= (:hit-target result-1) true))
      (is (= (:hit-target result-2) false))
      (is (= (:hit-target result-3) false))
      (is (= (:hit-target result-4) true))
      (is (= (:hit-target result-5)) true)))
  (testing "update-population-fitness fitness :steps"
    (with-redefs [sketch/config {:lifetime 100}] ; TODO dep-to config is bad
      (let [fitness-fn (fn [rocket] (sketch/fitness :steps rocket))
            rockets-1 [{:force-index 100}]
            result-1 (sketch/update-population-fitness rockets-1 fitness-fn)
            rockets-2 [{:force-index 90}]
            result-2 (sketch/update-population-fitness rockets-2 fitness-fn)
            rockets-3 [{:force-index 0}]
            result-3 (sketch/update-population-fitness rockets-3 fitness-fn)]
        (is (= (:fitness (first result-1)) 1)) ; min fitness = 1
        (is (= (:fitness (first result-2)) (* 10 10)))
        (is (= (:fitness (first result-3)) (* 100 100))))))
  (testing "update-population-fitness fitness :distance"
    (let [target {:location [100 100] :target-r 100}
          fitness-fn (fn [rocket] (sketch/fitness :distance rocket target))
          rockets-1 [{:location [100 100]}]
          result-1 (sketch/update-population-fitness rockets-1 fitness-fn)
          rockets-2 [{:location [103 104]}]
          result-2 (sketch/update-population-fitness rockets-2 fitness-fn)]
      (is (= (:fitness (first result-1)) 1))
      (is (= (:fitness (first result-2)) (/ 1 (* 5 5))))))
  (testing "update-population-fitness fitness :steps-and-distance"
    (with-redefs [sketch/config {:lifetime 100}] ; TODO dep-to config is bad
      (let [target {:location [100 100] :target-r 100}
            fitness-fn (fn [rocket] (sketch/fitness :steps-and-distance rocket target))
            rockets-1 [{:location [100 100] :force-index 100}]
            result-1 (sketch/update-population-fitness rockets-1 fitness-fn)
            rockets-2 [{:location [103 104] :force-index 90}]
            result-2 (sketch/update-population-fitness rockets-2 fitness-fn)]
        (is (= (:fitness (first result-1)) (+ 1 1)))
        (is (= (:fitness (first result-2)) (+ (* 10 10) (/ 1 (* 5 5)))))))))

(deftest test-reproduction
  (testing "reproduce-forces"
    (let [rocket-1 {:forces [[0.01 0.03] [-0.01 -0.03]] :fitness 0}
          rocket-2 {:forces [[0.02 0.02] [-0.02 -0.02]] :fitness 10}
          rocket-3 {:forces [[0.03 0.01] [-0.03 -0.01]] :fitness 100}
          max-fitness 100
          result-1 (sketch/reproduce-forces rocket-1 max-fitness)
          result-2 (sketch/reproduce-forces rocket-2 max-fitness)
          result-3 (sketch/reproduce-forces rocket-3 max-fitness)]
      (is (= (count result-1) 0))
      (is (= (count result-2) 10))
      (is (= (count result-3) 100))
      (is (every? #(= % (:forces rocket-2)) result-2))))
  (testing "reproduce-forces"
    (let [rocket-1 {:forces [[0.01 0.03] [-0.01 -0.03]] :fitness 0}
          rocket-2 {:forces [[0.02 0.02] [-0.02 -0.02]] :fitness 10}
          rocket-3 {:forces [[0.03 0.01] [-0.03 -0.01]] :fitness 100}
          result-1 (sketch/gen-mating-pool (vector rocket-1 rocket-2 rocket-3))]
      (is (not= (some #(= % (:forces rocket-1)) result-1) true))
      (is (= (some #(= % (:forces rocket-2)) result-1) true))
      (is (= (some #(= % (:forces rocket-3)) result-1) true))))
  (testing "select-next-population"
    (with-redefs [sketch/config {:rocket-r 5 :mutation-rate 0.01}] ; TODO dep-to config is bad
      (let [rocket-1 {:forces [[0.01 0.03] [-0.01 -0.03]] :fitness 0}
            rocket-2 {:forces [[0.02 0.02] [-0.02 -0.02]] :fitness 10}
            rocket-3 {:forces [[0.03 0.01] [-0.03 -0.01]] :fitness 100}
            result-1 (sketch/select-next-population (vector rocket-1 rocket-2 rocket-3) [100 100])]
        (is (= (count result-1) 3))))))
