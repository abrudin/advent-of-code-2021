(in-ns 'se.lixi.adventofcode2021.core)
(require '[clojure.string :as str])
(require '[clojure.set :as cset])

(defn parse-day-11
  [file]
  (let [lines (get-lines file)]
    (list (count (first lines)) (vec (map #(Character/digit ^char % 10) (apply concat lines))))))

(defn neighbors-diag [index row-length max]
  (concat (neighbors index row-length max)
          (filter some?
                  [(let [i (index-up index row-length)] (if (nil? i) nil (index-left i row-length)))
                   (let [i (index-up index row-length)] (if (nil? i) nil (index-right i row-length)))
                   (let [i (index-down index row-length max)] (if (nil? i) nil (index-left i row-length)))
                   (let [i (index-down index row-length max)] (if (nil? i) nil (index-right i row-length)))]))
  )

(defn do-flash [row-length, grid, no-flashes]
  (let [flashing (first (map first (filter (comp (partial < 9) second) (map-indexed vector grid))))]
    (if (nil? flashing)
      (list grid no-flashes)
      (let [new-grid (assoc (reduce
                              (fn [grid, neighbor]
                                (if (= 0 (nth grid neighbor)) grid (assoc grid neighbor (inc (nth grid neighbor)))))
                              grid (neighbors-diag flashing row-length (count grid))) flashing 0)]
        (do-flash row-length new-grid (inc no-flashes)))
      )))

(defn advance [row-length, gridflash]
  (let [increased (map inc (first gridflash))]
    (do-flash row-length (vec increased) (second gridflash))
    ))

(defn pretty-print [row-length, grid]
  (str "\n" (str/join "\n" (map (partial str/join "") (partition row-length grid))))
  )

(defn advance-if-not-synched [row-length, gridflash, step-index]
  (let [nextgridflash (advance row-length gridflash)]
    (if (every? (partial = 0) (first nextgridflash))
      (inc step-index)
      (advance-if-not-synched row-length nextgridflash (inc step-index))
      )))

(defn day11 [file]
  {:first  (let [[row-length, grid] (parse-day-11 file)]
             (second (reduce (fn [gridflash, _] (advance row-length gridflash)) (list grid 0) (range 100))))
   :second (let [[row-length, grid] (parse-day-11 file)]
             (advance-if-not-synched row-length (list grid 0) 0))})