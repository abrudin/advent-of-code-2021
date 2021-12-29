(in-ns 'se.lixi.adventofcode2021.core)
(require '[clojure.string :as str])

(defn step
  ([posx, velx, posy, vely, target]
   (let [[xmin, xmax, ymin, ymax] target]
     (let [[posxnew, posynew] [(+ posx velx) (+ posy vely)]]
       (let [[velxnew, velynew] [(max 0 (- velx 1)) (- vely 1)]]
         (if (and (>= posxnew xmin) (<= posxnew xmax) (>= posynew ymin) (<= posynew ymax))
           true
           (if (or (> posxnew xmax) (< posynew ymin)) false (recur posxnew, velxnew, posynew, velynew, target)))))))
  ([target vel] (step 0 (first vel) 0 (second vel) target)))

(defn parse-day-17
  [file]
  (let [xy (second (str/split (slurp (clojure.java.io/resource file)) #":"))]
    (let [[x y] (str/split xy #",")]
      (let [xrange (str/split (second (str/split x #"=")) #"\.\.")]
        (let [yrange (str/split (second (str/split y #"=")) #"\.\.")]
          [(parse-int (first xrange)) (parse-int (second xrange)) (parse-int (first yrange)) (parse-int (second yrange))]
          )
        ))))

(defn day17 [file]
  (let [target (parse-day-17 file)]
    {:first  (/ (* (nth target 2) (+ 1 (nth target 2))) 2)
     :second (count (filter (partial step target) (for [x (range 0 200) y (range -200 200)] (vector x y))))
     }))
