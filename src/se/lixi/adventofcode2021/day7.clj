(in-ns 'se.lixi.adventofcode2021.core)
(require '[clojure.string :as str])

(defn fuel-cost-if-lower [positions, mapping, prev, target]
  (let [cost (reduce + (map (comp mapping #(abs (- target %))) positions))]
    (if (or (nil? prev) (< cost prev)) cost prev)))

(defn calc-fuel-cost [positions, mapping]
  (reduce (partial fuel-cost-if-lower positions mapping) nil (range (apply max positions))))

(defn parse-day-7 [file] (map parse-int (str/split (first (get-lines file)) #",")))

(defn day7 [file]
  {:first  (calc-fuel-cost (parse-day-7 file) identity)
   :second (calc-fuel-cost (parse-day-7 file) #(/ (* % (+ 1 %)) 2))})