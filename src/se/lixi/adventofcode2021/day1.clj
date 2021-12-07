(in-ns 'se.lixi.adventofcode2021.core)
(defn increment-if-increase
  [a b]
  {:prev b :inc
   (+
     (get a :inc)
     (if (and (contains? a :prev) (< (get a :prev) b)) 1 0))})

(defn day1 [file]
  {:first  (get
             (reduce increment-if-increase
                     {:inc 0}
                     (get-lines-as-ints file)) :inc)
   :second (get
             (reduce increment-if-increase
                     {:inc 0}
                     (map
                       (partial reduce +)
                       (partition 3 1 (get-lines-as-ints file)))) :inc)
   })