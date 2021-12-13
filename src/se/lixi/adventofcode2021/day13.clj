(in-ns 'se.lixi.adventofcode2021.core)
(require '[clojure.string :as str])
(require '[clojure.set :as cset])

(defn parse-day-13
  [file]
  (let [[coordinates-str, instructions-str] (str/split (slurp (clojure.java.io/resource file)) #"\n\n")]
    (let [coordinates (set (map #(map parse-int (str/split % #",")) (str/split coordinates-str #"\n")))]
      (let [instructions (map #(str/split % #"=") (str/split instructions-str #"\n"))]
        (list (map #(list (first %) (parse-int (second %))) instructions) coordinates)))))

(defn fill-from [pair-list row-length paper]
  (if (empty? pair-list)
    paper
    (let [x (first (first pair-list))]
      (let [y (second (first pair-list))]
        (fill-from (rest pair-list) row-length (assoc paper (+ x (* y row-length)) "##"))))))

(defn pretty-print-paper [dots]
  (let [row-length (inc (apply max (map first dots)))]
    (let [number-of-rows (inc (apply max (map second dots)))]
      (let [grid (fill-from dots row-length (vec (repeat (* row-length number-of-rows) "..")))]
        (str "\n" (str/join "\n" (map (partial str/join "") (partition row-length grid))) "\n"))))
  )

(defn fold-along [dots, instruction]
  (let [fold-point (second instruction)]
    (case (first instruction)
      "fold along x" (set (map (fn [[x, y]] (if (> x fold-point) (list (- (* 2 fold-point) x) y) (list x y))) dots))
      "fold along y" (set (map (fn [[x, y]] (if (> y fold-point) (list x (- (* 2 fold-point) y)) (list x y))) dots)))))

(defn day13 [file]
  {:first  (let [[instructions, dots] (parse-day-13 file)]
             (count (fold-along dots (first instructions)))
             )
   :second (let [[instructions, dots] (parse-day-13 file)]
             (pretty-print-paper (reduce fold-along dots instructions))
             )})
