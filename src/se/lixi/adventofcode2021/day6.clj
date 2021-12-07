(in-ns 'se.lixi.adventofcode2021.core)
(require '[clojure.string :as str])

(defn parse-day-6 [file]
  (map (fn [i] (count (filter #(= % i) (map parse-int (str/split (first (get-lines file)) #","))))) (range 9)))

(defn advance [v _]
  (let [updated (vec (concat (rest v) [(first v)]))]
    (assoc updated 6 (+ (first v) (nth updated 6)))))

(defn day6 [file]
  (let [input (parse-day-6 file)]
    {:first  (apply + (reduce advance input (range 80)))
     :second (apply + (reduce advance input (range 256)))
     }))