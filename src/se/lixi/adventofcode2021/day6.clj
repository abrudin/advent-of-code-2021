(in-ns 'se.lixi.adventofcode2021.core)
(require '[clojure.string :as str])

(defn parse-day-6
  "Creates a (length-9) array with the frequency of n at index n"
  [file]
  (map (fn [i] (count (filter #(= % i) (map parse-int (str/split (first (get-lines file)) #","))))) (range 9)))

(defn advance [v _]
  "Shifts the array to the left (going around the corner) and adds the value at index 0 to index 6"
  (let [updated (vec (concat (rest v) [(first v)]))]
    (assoc updated 6 (+ (first v) (nth updated 6)))))

(defn day6 [file]
  (let [input (parse-day-6 file)]
    {:first  (apply + (reduce advance input (range 80)))
     :second (apply + (reduce advance input (range 256)))
     }))