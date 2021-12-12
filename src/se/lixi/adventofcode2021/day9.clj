(in-ns 'se.lixi.adventofcode2021.core)
(require '[clojure.string :as str])
(require '[clojure.set :as cset])

(defn parse-day-9
  [file]
  (let [lines (get-lines file)]
    (list (count (first lines)) (vec (map #(Character/digit ^char % 10) (apply concat lines))))))

(defn index-up [index, row-length]
  (let [new-index (- index row-length)]
    (if (>= new-index 0) new-index)))

(defn index-right [index, row-length]
  (if (not (= (mod (inc index) row-length) 0)) (inc index)))

(defn index-down [index, row-length, total-count]
  (let [new-index (+ index row-length)]
    (if (< new-index total-count) new-index)))

(defn index-left [index, row-length]
  (if (not (= (mod index row-length) 0)) (dec index))
  )

(defn neighbors [index row-length max]
  (filter some?
          [(index-up index row-length)
           (index-right index row-length)
           (index-down index row-length max)
           (index-left index row-length)])
  )

(defn find-lows-indices
  [row-length, input]
  (filter
    (fn [index] (every?
                  #(< (nth input index) %)
                  (map #(nth input %) (neighbors index row-length (count input)))))
    (range 0 (count input))))

(defn find-lows
  [row-length, input]
  (map #(nth input %) (find-lows-indices row-length input)))

(defn bubble
  ([queue, bubbled, input, row-length]
   (if (empty? queue)
     bubbled
     (let [index (first queue)]
       (let [nbs (neighbors index row-length (count input))]
         (bubble (concat
                   (rest queue)
                   (filter #(and (not (contains? bubbled %)) (not (= (nth input %) 9)))
                           nbs)) (conj bubbled index) input row-length)
         ))))
  ([low, input, row-length]
   (bubble (list low) #{} input row-length)))

(defn day9 [file]
  (let [[row-length, input] (parse-day-9 file)]
    {:first  (apply + (map inc (find-lows row-length input)))
     :second (apply
               *
               (take
                 3
                 (reverse
                   (sort
                     (map
                       #(count (bubble % input row-length))
                       (find-lows-indices row-length input))))))
     }))