(in-ns 'se.lixi.adventofcode2021.core)
(require '[clojure.string :as str])

(defn follow-direction-simple [a, b]
  (let [word (get b :word)]
    (let [amount (get b :amount)]
      {:hor (+ (get a :hor) (case word "forward" amount 0))
       :dep (+ (get a :dep) (case word "forward" 0 "up" (* -1 amount) "down" amount))})))

(defn follow-direction-advanced [a, b]
  (let [word (get b :word)]
    (let [amount (get b :amount)]
      (let [aim (get a :aim)]
        {:hor (+ (get a :hor) (case word "forward" amount 0))
         :dep (+ (get a :dep) (case word "forward" (* aim amount) 0))
         :aim (+ aim (case word "up" (* -1 amount) "down" amount 0))}))))

(defn get-direction [dir]
  (let [instr (str/split dir #" ")]
    {:word   (nth instr 0)
     :amount (Integer/parseInt (nth instr 1))}))

(defn follow-direction
  [file, dir-fn]
  (reduce dir-fn
          {:hor 0 :dep 0}
          (map get-direction (get-lines file))))

(defn day2 [file]
  {:first  (let [target (follow-direction file follow-direction-simple)]
             (* (get target :hor) (get target :dep)))
   :second (let [target (reduce follow-direction-advanced
                                {:aim 0 :hor 0 :dep 0}
                                (map get-direction (get-lines file)))]
             (* (get target :hor) (get target :dep)))})