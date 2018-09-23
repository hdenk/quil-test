(ns nature-of-code.genetic-algorithms.smart-rockets-superbasic.sketch-test
  (:require [cljs.test :refer-macros [deftest is testing run-tests]]
            [nature-of-code.genetic-algorithms.smart-rockets-superbasic.sketch :as sketch]))

(deftest test-crossover
  (let [dna-length 16
        dna1 (sketch/random-dna dna-length)
        dna2 (sketch/random-dna dna-length)
        resulting-dna (sketch/crossover dna1 dna2)
        resulting-genes (:genes resulting-dna)]
    (is (= (count resulting-genes) dna-length))))
