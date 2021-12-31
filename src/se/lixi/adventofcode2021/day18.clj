(in-ns 'se.lixi.adventofcode2021.core)
(require '[clojure.string :as str])

(defn snailfish-add [left, right] {:left left :right right})

(defn snailfish-number
  ([tree, trail, remaining]
   (if (empty? remaining)
     (get tree :root)
     (let [[c & rest] remaining]
       (case c
         \[ (snailfish-number tree (conj trail :left) rest)
         \, (snailfish-number tree (conj (pop trail) :right) rest)
         \] (snailfish-number tree (pop trail) rest)
         (snailfish-number (assoc-in tree trail (Character/digit ^char c 10)) trail rest)
         )
       )
     ))
  ([remaining] (snailfish-number nil [:root] remaining))
  )

(defn snailfish-print [tree]
  (if (number? tree)
    (str tree)
    (str "[" (snailfish-print (get tree :left)) "," (snailfish-print (get tree :right)) "]")))

(defn find-split
  ([tree, trail]
   (if (number? (get-in tree trail))
     (if (> (get-in tree trail) 9) trail nil)
     (let [explode-left (find-split tree (conj trail :left))]
       (if (nil? explode-left)
         (let [explode-right (find-split tree (conj trail :right))]
           (if (nil? explode-right) nil explode-right))
         explode-left))))
  ([tree] (find-split tree []))
  )

(defn find-explode
  ([tree, trail]
   (if (number? (get-in tree trail))
     nil
     (if (> (count trail) 3)
       trail
       (let [explode-left (find-explode tree (conj trail :left))]
         (if (nil? explode-left)
           (let [explode-right (find-explode tree (conj trail :right))]
             (if (nil? explode-right) nil explode-right))
           explode-left)))))
  ([tree] (find-explode tree []))
  )

(defn dig-left [tree trail] (if (number? (get-in tree trail)) trail (dig-left tree (conj trail :left))))
(defn dig-right [tree trail] (if (number? (get-in tree trail)) trail (dig-right tree (conj trail :right))))

(defn get-first-left [tree, trail]
  (let [remaining (vec (reverse (drop-while (partial = :left) (reverse trail))))]
    (if (empty? remaining) [] (dig-right tree (conj (pop remaining) :left)))))

(defn get-first-right [tree, trail]
  (let [remaining (vec (reverse (drop-while (partial = :right) (reverse trail))))]
    (if (empty? remaining) [] (dig-left tree (conj (pop remaining) :right)))))

(defn snailfish-reduce [tree]
  (let [explode-trail (find-explode tree)]
    (if (nil? explode-trail)
      (let [split-trail (find-split tree)]
        (if (nil? split-trail)
          tree
          (let [value (get-in tree split-trail)]
            (snailfish-reduce (assoc-in tree split-trail {:left (quot value 2) :right (int (Math/ceil (/ value 2)))})))))
      (let [[left-trail, right-trail] [(get-first-left tree explode-trail) (get-first-right tree explode-trail)]]
        (let [left-replaced (if (empty? left-trail) tree (assoc-in tree left-trail (+ (get-in tree left-trail) (get-in tree (conj explode-trail :left)))))]
          (let [right-replaced (if (empty? right-trail) left-replaced (assoc-in left-replaced right-trail (+ (get-in tree right-trail) (get-in tree (conj explode-trail :right)))))]
            (snailfish-reduce (assoc-in right-replaced explode-trail 0))
            ))
        )
      )))
(defn parse-day-18
  [file]
  (map snailfish-number (str/split (slurp (clojure.java.io/resource file)) #"\n")))

(defn snailfish-add-and-reduce [left, right]
  (snailfish-reduce (snailfish-add left right)))

(defn snailfish-magnitude [tree]
  (if (number? tree)
    tree
    (+ (* 3 (snailfish-magnitude (get tree :left))) (* 2 (snailfish-magnitude (get tree :right))))))

(defn day18 [file]
  (let [target (parse-day-18 file)]
    {:first  (snailfish-magnitude (reduce snailfish-add-and-reduce (first target) (rest target)))
     :second (apply max (map #(snailfish-magnitude (snailfish-add-and-reduce (nth target (first %)) (nth target (second %)))) (filter #(not (= (first %) (second %))) (for [x (range 0 (- (count target) 1)) y (range 0 (- (count target) 1))] (vector x y)))))
     }
    )
  )
