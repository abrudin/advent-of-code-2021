(in-ns 'se.lixi.adventofcode2021.core)
(require '[clojure.string :as str])
(require '[clojure.set :as cset])
(require '[clojure.data.priority-map :refer [priority-map]])

(defn parse-day-15
  [file]
  (let [rows (str/split (slurp (clojure.java.io/resource file)) #"\n")]
    (let [width (count (first rows))]
      [width (map parse-int (str/split (str/join rows) #""))])))

(defn neighbors15 [index width input]
  (filter some?
          [(if (>= (- index width) 0) (- index width))
           (if (not (= (mod (inc index) width) 0)) (inc index))
           (if (< (+ index width) (count input)) (+ index width))
           (if (not (= (mod index width) 0)) (dec index))])
  )

(defn update-queue [zs, Du, labels, queue, input]
  (if (empty? zs)
    [labels queue]
    (let [z (first zs)]
      (let [w (+ Du (nth input z))]
        (let [updated-labels (if (< w (get labels z)) (assoc labels z w) labels)]
          (let [updated-queue (if (< w (get queue z)) (assoc queue z w) queue)]
            (recur (rest zs) Du updated-labels updated-queue input)
            ))))))

(defn dijkstra
  ([width, input, labels, queue]
   (if (empty? queue)
     labels
     (let [[u, Du] (first queue)]
       (let [new-queue (dissoc queue u)]
         (let [[updated-labels updated-queue] (update-queue (filter #(contains? new-queue %) (neighbors15 u width input)) Du labels new-queue input)]
           (recur width input updated-labels updated-queue)))))
   ))

(defn inc-wrap [ns i]
  (map
    #(let [w (+ % i)]
       (if (<= w 9) w (- w 9)))
    ns))

(defn multiply-row [row]
  (apply concat (map (partial inc-wrap row) (range 5))))

(defn multiply-tile-hor [width input]
  (let [output (mapcat #(multiply-row (take width (drop (* % width) input))) (range width))]
    [(* width 5) output]))

(defn multiply-tile [width input]
  (let [[new-width new-input] (multiply-tile-hor width input)]
    [new-width (multiply-row new-input)])
  )

(defn day15 [file]
  {:first  (let [[width input] (parse-day-15 file)]
             (let [data (concat [[0 0]] (map #(vec [% Long/MAX_VALUE]) (range 1 (count input))))]
               (get (dijkstra width input (into {} data) (apply priority-map (into [] (flatten data)))) (- (count input) 1))))
   :second (let [[tile-width input-tile] (parse-day-15 file)]
             (let [[width input] (multiply-tile tile-width input-tile)]
               (let [data (concat [[0 0]] (map #(vec [% Long/MAX_VALUE]) (range 1 (count input))))]
                 (get (dijkstra width input (into {} data) (apply priority-map (into [] (flatten data)))) (- (count input) 1)))))
   })
