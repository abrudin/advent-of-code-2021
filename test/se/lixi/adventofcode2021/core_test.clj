(ns se.lixi.adventofcode2021.core-test
  (:require [clojure.test :refer :all]
            [se.lixi.adventofcode2021.core :refer :all]))

(deftest day1-test
  (testing
    (let [output (day1 "day1.txt")]
      (is (= 1466 (get output :first)))
      (is (= 1491 (get output :second))))))

(deftest day2-test
  (testing
    (let [output (day2 "day2.txt")]
      (is (= 1840243 (get output :first)))
      (is (= 1727785422 (get output :second))))))

(deftest day3-test
  (testing
    (let [output (day3 "day3.txt")]
      (is (= 1071734 (get output :first)))
      (is (= 6124992 (get output :second))))))

(deftest day4-test
  (testing
    (let [output (day4 "day4.txt")]
      (is (= 38913 (get output :first)))
      (is (= 16836 (get output :second))))))

(deftest day5-test
  (testing
    (let [output (day5 "day5.txt")]
      (is (= 5145 (get output :first)))
      (is (= 16518 (get output :second))))))

(deftest day6-test
  (testing
    (let [output (day6 "day6.txt")]
      (is (= 373378 (get output :first)))
      (is (= 1682576647495 (get output :second))))))

(deftest day7-test
  (testing
    (let [output (day7 "day7.txt")]
      (is (= 348664 (get output :first)))
      (is (= 100220525 (get output :second))))))

(deftest day8-test
  (testing
    (let [output (day8 "day8.txt")]
      (is (= 409 (get output :first)))
      (is (= 1024649 (get output :second))))))

(deftest day9-test
  (testing
    (let [output (day9 "day9.txt")]
      (is (= 496 (get output :first)))
      (is (= 902880 (get output :second))))))

(deftest day10-test
  (testing
    (let [output (day10 "day10.txt")]
      (is (= 268845 (get output :first)))
      (is (= 4038824534 (get output :second))))))

(deftest day11-test
  (testing
    (let [output (day11 "day11.txt")]
      (is (= 1608 (get output :first)))
      (is (= 214 (get output :second))))))

(deftest day12-test
  (testing
    (let [output (day12 "day12.txt")]
      (is (= 4241 (get output :first)))
      (is (= 122134 (get output :second))))))

(def s
  "
##....##..########....####....######....########..##....##..######....######..
##....##..##........##....##..##....##........##..##..##....##....##..##....##
########..######....##........##....##......##....####......##....##..##....##
##....##..##........##........######......##......##..##....######....######..
##....##..##........##....##..##..##....##........##..##....##........##..##..
##....##..########....####....##....##..########..##....##..##........##....##
")
(deftest day13-test
  (testing
    (let [output (day13 "day13.txt")]
      (is (= 759 (get output :first)))
      (is (= s (get output :second))))))

(deftest day14-test
  (testing
    (let [output (day14 "day14.txt")]
      (is (= 3831 (get output :first)))
      (is (= 5725739914282 (get output :second))))))

; Too inefficient - but eventually terminates
;(deftest day15-test
;  (testing
;    (let [output (day15 "day15.txt")]
;      (is (= 553 (get output :first)))
;      (is (= 2858 (get output :second)))
;      )))

(deftest day16-test
  (testing
    (let [output (day16 "day16.txt")]
      (is (= 843 (get output :first)))
      (is (= 5390807940351 (get output :second)))
      )))

(deftest day17-test
  (testing
    (let [output (day17 "day17.txt")]
      (is (= 9180 (get output :first)))
      (is (= 3767 (get output :second)))
      )))