(ns se.lixi.adventofcode2021.core-test
  (:require [clojure.test :refer :all]
            [se.lixi.adventofcode2021.core :refer :all]))

(deftest day1-test
  (testing
    (is (= 1466 (get (day1 "day1.txt") :first)))
    (is (= 1491 (get (day1 "day1.txt") :second)))))

(deftest day2-test
  (testing
    (is (= 1840243 (get (day2 "day2.txt") :first)))
    (is (= 1727785422 (get (day2 "day2.txt") :second)))))

(deftest day3-test
  (testing
    (is (= 1071734 (get (day3 "day3.txt") :first)))
    (is (= 6124992 (get (day3 "day3.txt") :second)))))

(deftest day4-test
  (testing
    (is (= 38913 (get (day4 "day4.txt") :first)))
    (is (= 16836 (get (day4 "day4.txt") :second)))))

(deftest day5-test
  (testing
    (is (= 5145 (get (day5 "day5.txt") :first)))
    (is (= 16518 (get (day5 "day5.txt") :second)))))

(deftest day6-test
  (testing
    (is (= 373378 (get (day6 "day6.txt") :first)))
    (is (= 1682576647495 (get (day6 "day6.txt") :second)))))

(deftest day7-test
  (testing
    (is (= 348664 (get (day7 "day7.txt") :first)))
    (is (= 100220525 (get (day7 "day7.txt") :second)))))
