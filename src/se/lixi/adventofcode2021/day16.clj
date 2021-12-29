(in-ns 'se.lixi.adventofcode2021.core)
(require '[clojure.string :as str])

(declare parse-packet)

(defn parse-day-16
  [file]
  (slurp (clojure.java.io/resource file)))

(defn to-int [lst] (Long/parseLong (str/join lst) 2))

(defn parse-literal-body
  ([parsed, bits-consumed, rest]
   (case (first rest)
     \0 [(to-int (flatten (conj parsed (take 4 (drop 1 rest))))) (+ 5 bits-consumed)]
     \1 (parse-literal-body (conj parsed (take 4 (drop 1 rest))) (+ 5 bits-consumed) (drop 5 rest))
     )
   )
  ([body] (parse-literal-body [] 0 body)))

(defn parse-literal [packet]
  (let [[value, bits-consumed] (parse-literal-body (drop 6 packet))]
    [{:version (to-int (take 3 packet))
      :type-id (to-int (take 3 (drop 3 packet)))
      :value   value
      }, (+ 6 bits-consumed)]
    ))

(defn parse-n-packages
  ([parsed, bits-consumed, rest, remaining]
   (if (= 0 remaining)
     [parsed, bits-consumed]
     (let [[p, consumed] (parse-packet rest)]
       (parse-n-packages
         (conj parsed p)
         (+ consumed bits-consumed)
         (drop consumed rest)
         (- remaining 1)))))
  ([body, quantity] (parse-n-packages [] 0 body quantity)))

(defn parse-packages-of-length
  ([parsed, bits-consumed, rest, remaining]
   (if (= 0 remaining)
     [parsed, bits-consumed]
     (let [[p, consumed] (parse-packet rest)]
       (parse-packages-of-length
         (conj parsed p)
         (+ consumed bits-consumed)
         (drop consumed rest)
         (- remaining consumed))))
   )
  ([body, length] (parse-packages-of-length [] 0 body length))
  )

(defn parse-operator [packet]
  (let [[length-type-id & rest] (drop 6 packet)]
    (case length-type-id
      \0 (let [[sub-packets, bits-consumed] (parse-packages-of-length (drop 15 rest) (to-int (take 15 rest)))]
           [{:version        (to-int (take 3 packet))
             :type-id        (to-int (take 3 (drop 3 packet)))
             :length-type-id length-type-id
             :length         (to-int (take 15 rest))
             :sub-packets    sub-packets
             } (+ 22 bits-consumed)]
           )
      \1 (let [[sub-packets, bits-consumed] (parse-n-packages (drop 11 rest) (to-int (take 11 rest)))]
           [{:version        (to-int (take 3 packet))
             :type-id        (to-int (take 3 (drop 3 packet)))
             :length-type-id length-type-id
             :length         (to-int (take 11 rest))
             :sub-packets    sub-packets
             } (+ 18 bits-consumed)]))
    ))

(defn parse-packet [packet]
  (case (to-int (take 3 (drop 3 packet)))
    4 (parse-literal packet)
    (parse-operator packet)))

(defn sum-versions [tree]
  (if (contains? tree :sub-packets)
    (+ (get tree :version) (apply + (map sum-versions (get tree :sub-packets))))
    (get tree :version)
    )
  )

(defn calculate-value [tree]
  (let [sub-values (map calculate-value (get tree :sub-packets))]
    (case (get tree :type-id)
      0 (apply + sub-values)
      1 (apply * sub-values)
      2 (apply min sub-values)
      3 (apply max sub-values)
      4 (get tree :value)
      5 (if (apply > sub-values) 1 0)
      6 (if (apply < sub-values) 1 0)
      7 (if (apply = sub-values) 1 0)
      ))
  )


(defn day16 [file]
  (letfn [(to-binary [letter] (str/replace (format "%4s" (Integer/toBinaryString (Character/digit ^char letter 16))) #"\s" "0"))]
    (let [parsed-package (first (parse-packet (str/join (map to-binary (parse-day-16 file)))))]
      {:first  (sum-versions parsed-package)
       :second (calculate-value parsed-package)
       })))
