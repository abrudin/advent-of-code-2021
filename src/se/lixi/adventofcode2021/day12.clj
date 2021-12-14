(in-ns 'se.lixi.adventofcode2021.core)
(require '[clojure.string :as str])
(require '[clojure.set :as cset])

(defn parse-day-12
  [file]
  (map #(str/split % #"-") (get-lines file)))

(defn cave-map-from [coordinates]
  (dissoc
    (map-on-map
      (merge-with into
                  (map-on-map (group-by first coordinates) (partial map second))
                  (map-on-map (group-by second coordinates) (partial map first)))
      (fn [as] (apply vector (filter (partial not= "start") as))))
    "end")
  )

(defn max-one-of-each-small [arr]
  (let [a (frequencies arr)]
    (let [lower-keys (filter #(Character/isLowerCase ^char (first %)) (keys a))]
      (every? #(<= (get a %) 1) lower-keys
              ))))

(defn max-two-of-one-small-and-one-of-rest [arr]
  (let [a (frequencies arr)]
    (let [lower-keys (filter #(Character/isLowerCase ^char (first %)) (keys a))]
      (and (<= (count (filter #(= 2 (get a %)) lower-keys)) 1)
           (every? #(<= (get a %) 2) lower-keys
                   )))))

(defn walk-caves [start-from, finished, cave-map, small-caves-fn]
  (if (empty? start-from)
    finished
    (let [nexts (mapcat (fn [row] (map (partial conj row) (get cave-map (last row)))) start-from)]
      (let [finisheds (filter (comp (partial = "end") last) nexts)]
        (let [remaining (filter #(and (small-caves-fn %) (not= "end" (last %))) nexts)]
          (walk-caves remaining (concat finisheds finished) cave-map small-caves-fn)
          ))))
  )

(defn day12 [file]
  (let [coordinates (parse-day-12 file)]
    (let [cave-map (cave-map-from coordinates)]
      (let [start (vector (vector "start"))]
        {:first  (count (walk-caves start () cave-map max-one-of-each-small))
         :second (count (walk-caves start () cave-map max-two-of-one-small-and-one-of-rest))}))))
