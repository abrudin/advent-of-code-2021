(in-ns 'se.lixi.adventofcode2021.core)
(require '[clojure.string :as str])

(defn find-common [seq, index, high]
  (let [half-num-lines (/ (count seq) 2)]
    (let [most-common-line (reduce (partial map +) (map #(map parse-int (str/split % #"")) seq))]
      (let [at-index (nth most-common-line index)]
        (str
          (if (>= at-index half-num-lines) (if high 1 0) (if high 0 1)))))))

(defn keep-common [seq, index, high]
  (let [most-common (find-common seq index high)]
    (filter #(= (subs % index (+ index 1)) most-common) seq))
  )

(defn keep-most-common
  ([seq, index, high]
   (let [most-common (keep-common seq index high)]
     (if (> (count most-common) 1) (keep-most-common most-common (+ index 1) high) most-common)
     ))
  ([seq, high] (keep-most-common seq 0 high)))

(defn day3 [file]
  {:first  (let [ge (let [lines (get-lines file)]
                      (let [of-each (reduce (partial map +)
                                            (map #(map parse-int (str/split % #"")) lines))]
                        (let [half-num-lines (/ (count lines) 2)]
                          {:gamma   (str/join "" (map #(if (> % half-num-lines) 1 0) of-each))
                           :epsilon (str/join "" (map #(if (<= % half-num-lines) 1 0) of-each))})))]
             (* (Integer/parseInt (get ge :gamma) 2)
                (Integer/parseInt (get ge :epsilon) 2)))
   :second (let [lines (get-lines file)]
             (let [oxygen (keep-most-common lines Boolean/TRUE)]
               (let [co2 (keep-most-common lines Boolean/FALSE)]
                 (* (Integer/parseInt (str/join "" oxygen) 2)
                    (Integer/parseInt (str/join "" co2) 2)))))})