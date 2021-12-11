(in-ns 'se.lixi.adventofcode2021.core)
(require '[clojure.string :as str])
(require '[clojure.set :as cset])

(defn parse-day-8
  "Creates a (length-9) array with the frequency of n at index n"
  [file]
  (map (fn [line] (map #(str/split (str/trim %) #" ") (str/split line #"\|"))) (get-lines file)))

(defn count-length [length output] (count (filter #(= length (count %)) output)))

(def criterias
  {
   1 (fn [_, remaining] (= 2 (count remaining)))            ; only remaining digit with two segments
   4 (fn [_, remaining] (= 4 (count remaining)))            ; only remaining digit with four segments
   7 (fn [_, remaining] (= 3 (count remaining)))            ; only remaining digit with three segments
   8 (fn [_, remaining] (= 7 (count remaining)))            ; only remaining digit with seven segments
   9 (fn [foundVals, remaining] (every? #(contains? (set remaining) %) (get foundVals 4))) ; only remaining digit with all segments that four has
   0 (fn [foundVals, remaining]
       (and
         (= 6 (count remaining))
         (every? #(contains? (set remaining) %) (get foundVals 1)))) ; only remaining digit with six segments and all segments that one has
   6 (fn [_, remaining] (= 6 (count remaining)))            ; only remaining digit with six segments
   3 (fn [foundVals, remaining] (every? #(contains? (set remaining) %) (get foundVals 1))) ; only remaining digit with all segment that one has
   5 (fn [foundVals, remaining] (every? #(contains? (set (get foundVals 6)) %) remaining)) ; only remaining digit where six has all of its segment
   2 (fn [_, remaining] (first remaining))                  ; only remaining digit
   })

(defn findn [digit, wires]
  (let [digit-fn (partial (get criterias digit) (clojure.set/map-invert (first wires)))]
    (list
      (assoc (first wires) (sort (first (filter digit-fn (second wires)))) digit)
      (vec (filter (complement digit-fn) (second wires))))))

(defn find-all
  ([wires, remaining-digits]
   (if (empty? remaining-digits)
     (first wires)
     (find-all (findn (first remaining-digits) wires) (rest remaining-digits))))
  ([wires] (find-all (list {} wires) '(1 4 7 8 9 0 6 3 5 2))))

(defn find-number [row]
  (let [digits (find-all (first row))]
    (apply + (map * [1000 100 10 1] (map #(get digits (sort %)) (second row))))
    ))

(defn day8 [file]
  (let [input (parse-day-8 file)]
    {:first  (apply + (map #(apply + (map (partial count-length %) (map second input))) [2 3 4 7]))
     :second (apply + (map find-number input))
     }))