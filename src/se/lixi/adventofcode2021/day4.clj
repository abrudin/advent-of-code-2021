(in-ns 'se.lixi.adventofcode2021.core)
(require '[clojure.string :as str])

(defn index-of [item, coll]
  (let [idx (count (take-while (partial not= item) coll))]
    (if (< idx (count coll)) idx false)))

(defn bingo [board, idx]
  (let [row (quot idx 5)]
    (let [col (mod idx 5)]
      (or (every? #(= -1 (nth board (+ (* 5 row) %))) (take 5 (range)))
          (every? #(= -1 (nth board (+ (* 5 %) col))) (take 5 (range)))))))

(defn cross-on-one [number, board]
  (let [idx (index-of number board)]
    (if (not idx)
      {:board board :bingo false}
      (let [new-board (assoc board idx -1)]
        {:board new-board
         :bingo (bingo new-board idx)}))))

(defn cross-until-win
  ([numbers, boards, index]
   (let [curr-number (nth numbers index)]
     (let [new-boards (map (partial cross-on-one curr-number) boards)]
       (let [win-board (first (filter #(get % :bingo) new-boards))]
         (if (some? win-board)
           {:board (get win-board :board) :number curr-number}
           (cross-until-win numbers (map #(get % :board) new-boards) (+ 1 index)))))))
  ([numbers, boards] (cross-until-win numbers boards 0)))

(defn cross-until-one-left
  ([numbers, boards, index]
   (let [curr-number (nth numbers index)]
     (let [new-boards (map (partial cross-on-one curr-number) boards)]
       (if (and (= 1 (count new-boards)) (get (first new-boards) :bingo))
         {:board (get (first new-boards) :board) :number curr-number}
         (let [lose-boards (filter #(not (get % :bingo)) new-boards)]
           (cross-until-one-left numbers (map #(get % :board) lose-boards) (+ 1 index)))))))
  ([numbers, boards] (cross-until-one-left numbers boards 0)))

(defn parse-line [line]
  (map parse-int
       (filter #(not (str/blank? %))
               (str/split line #" "))))

(defn parse-input [lines]
  {:numbers (map parse-int (str/split (first lines) #","))
   :boards
   (vec (map vec (map flatten (partition 5
                                         (map parse-line
                                              (rest (filter #(not (str/blank? %)) lines)))))))
   })

(defn day4 [file]
  {:first  (let [input (parse-input (get-lines file))]
             (let [bingo-board (cross-until-win (get input :numbers) (get input :boards))]
               (* (reduce + (filter (partial not= -1) (get bingo-board :board))) (get bingo-board :number))))
   :second (let [input (parse-input (get-lines file))]
             (let [bingo-board (cross-until-one-left (get input :numbers) (get input :boards))]
               (println bingo-board)
               (* (reduce + (filter (partial not= -1) (get bingo-board :board))) (get bingo-board :number))))})