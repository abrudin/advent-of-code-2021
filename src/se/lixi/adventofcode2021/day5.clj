(in-ns 'se.lixi.adventofcode2021.core)
(require '[clojure.string :as str])

(defn idx-from [point, size] (+ (first point) (* (second point) size)))

(defn range-inclusive [start end] (if (<= start end) (range start (inc end)) (range start (dec end) -1)))

(defn apply-point
  ([surface, point] (apply-point surface point (int (Math/sqrt (count surface)))))
  ([surface, point, size] (assoc surface (idx-from point size) (inc (nth surface (idx-from point size))))))

(defn expand-points
  [include-diagonals start end]
  (let [[xs ys] (map list start end)]
    (cond
      (apply = xs) (map list (repeat (inc (abs (apply - ys))) (first xs)) (apply range-inclusive ys))
      (apply = ys) (map list (apply range-inclusive xs) (repeat (inc (abs (apply - xs))) (first ys)))
      :else (if include-diagonals (map list (apply range-inclusive xs) (apply range-inclusive ys)) ()))))

(defn parse-day-5 [lines]
  (let [parse-line (fn [line] (map #(map parse-int (str/split % #",")) (str/split line #" -> ")))]
    (vec (map parse-line lines))))

(defn apply-all-lines [input include-diagonals]
  (reduce apply-point
          (vec (repeat 1000000 0))
          (mapcat (partial expand-points include-diagonals) (map first input) (map second input))))

(defn day5 [file]
  (let [input (parse-day-5 (get-lines file))]
    (let [two-or-more (fn [surface] (count (filter (partial <= 2) surface)))]
      {:first  (two-or-more (apply-all-lines input false))
       :second (two-or-more (apply-all-lines input true))
       })))