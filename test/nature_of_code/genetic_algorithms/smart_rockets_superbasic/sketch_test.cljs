(ns nature-of-code.genetic-algorithms.smart-rockets-superbasic.sketch-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [nature-of-code.genetic-algorithms.smart-rockets-superbasic.sketch :as sketch]))

(deftest test-dna
  (testing "crossover"
    (let [dna {:genes [1 1 1 1 1 1 1 1]}
          partner-dna {:genes [2 2 2 2 2 2 2 2]}
          resulting-genes-0 (:genes (sketch/crossover dna partner-dna 0))
          resulting-genes-1 (:genes (sketch/crossover dna partner-dna 1))
          resulting-genes-4 (:genes (sketch/crossover dna partner-dna 4))
          resulting-genes-7 (:genes (sketch/crossover dna partner-dna 7))
          resulting-genes-8 (:genes (sketch/crossover dna partner-dna 8))]
      (is (= (count resulting-genes-4) (count (:genes dna)) (count (:genes partner-dna))))
      (is (= resulting-genes-0 [2 2 2 2 2 2 2 2]))
      (is (= resulting-genes-1 [1 2 2 2 2 2 2 2]))
      (is (= resulting-genes-4 [1 1 1 1 2 2 2 2]))
      (is (= resulting-genes-7 [1 1 1 1 1 1 1 2]))
      (is (= resulting-genes-8 [1 1 1 1 1 1 1 1])))))
