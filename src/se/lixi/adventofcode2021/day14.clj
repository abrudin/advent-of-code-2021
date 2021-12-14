(in-ns 'se.lixi.adventofcode2021.core)
(require '[clojure.string :as str])
(require '[clojure.set :as cset])

(defn parse-day-14
  [file]
  (let [[template, rules-str] (str/split (slurp (clojure.java.io/resource file)) #"\n\n")]
    (let [rules (map #(str/split % #" -> ") (str/split rules-str #"\n"))]
      (list (map str (seq template)) (into {} rules)))))

(defn process [rules, template, _]
  (filter some? (mapcat #(list (first %) (get rules (str/join %))) (partition 2 1 [false] template))))

(defn process-2 [rules, template, _]
  (reduce (fn [news, key]
            (let [new (get rules key)]
              (update
                (update
                  (update news key #(- % (get template key)))
                  (str/join [(first key) new])
                  #(+ (get template key) (if (nil? %) 0 %)))
                (str/join [new (second key)])
                #(+ (get template key) (if (nil? %) 0 %))
                )
              )) template (keys template))
  )

(defn map-on-map [m, f] (reduce (fn [new, [k v]] (assoc new k (f v))) {} m))

(defn create-template-from [template]
  (list (list (.charAt (first template) 0) (.charAt (last template) 0)) (frequencies (map str/join (partition 2 1 template)))))


(defn day14 [file]
  {:first  (let [[template, rules] (parse-day-14 file)]
             (let [values (vals (frequencies (reduce (partial process rules) template (range 10))))]
               (- (apply max values) (apply min values))))
   :second (let [[template, rules] (parse-day-14 file)]
             (let [templ (create-template-from template)]
               (let [values (vals (map-on-map
                                    (update
                                      (update
                                        (reduce (fn [res, entry]
                                                  (update
                                                    (update res (first (first entry)) #(+ (second entry) (if (nil? %) 0 %)))
                                                    (second (first entry))
                                                    #(+ (second entry) (if (nil? %) 0 %)))
                                                  ) {} (reduce (partial process-2 rules) (second templ) (range 40))) (first (first templ)) inc) (second (first templ)) inc)
                                    #(/ % 2)
                                    ))]
                 (- (apply max values) (apply min values))
                 )
               ))})
