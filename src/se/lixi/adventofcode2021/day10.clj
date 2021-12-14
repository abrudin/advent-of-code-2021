(in-ns 'se.lixi.adventofcode2021.core)
(require '[clojure.string :as str])
(require '[clojure.set :as cset])

(defn- find-error [line]
  (reduce (fn [stack char]
            (if (number? stack)
              stack
              (case char
                \( (conj stack \()
                \[ (conj stack \[)
                \{ (conj stack \{)
                \< (conj stack \<)
                \) (if (= (first stack) \() (pop stack) 3)
                \] (if (= (first stack) \[) (pop stack) 57)
                \} (if (= (first stack) \{) (pop stack) 1197)
                \> (if (= (first stack) \<) (pop stack) 25137)
                )))
          '()
          (seq line))
  )

(defn day10 [file]
  {:first  (apply + (filter number? (map find-error (get-lines file))))
   :second (let [result
                 (sort
                   (map
                     #(reduce (fn [total, char] (+ (case char \( 1 \[ 2 \{ 3 \< 4) (* 5 total))) 0 %)
                     (filter (complement number?) (map find-error (get-lines file)))))]
             (nth result (/ (count result) 2)))})