(in-ns 'se.lixi.adventofcode2021.core)
(require '[clojure.string :as str])

(defn parse-int [i] (Integer/parseInt i))
(defn abs [n] (max n (-' n)))
(defn blank? [i] (str/blank? i))

(defn get-lines-as-ints [file]
  (with-open [rdr (clojure.java.io/reader (.getFile (clojure.java.io/resource file)))]
    (doall (map parse-int (line-seq rdr)))))

(defn get-lines [file]
  (with-open [rdr (clojure.java.io/reader (.getFile (clojure.java.io/resource file)))]
    (doall (line-seq rdr))))
