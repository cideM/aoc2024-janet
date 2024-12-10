(def parser (peg/compile
              ~(split :s (any (group (* (line) (column) (number :d)))))))

(defn find-paths [g from target-value &opt seen]
  (default seen @{})
  (if (= (get g from) target-value)
    [[from]]
    (let [seen_ (merge-into (table/clone seen) {from true})
          neighbors (seq [[dy dx] :in [[-1 0] [0 1] [1 0] [0 -1]]
                          :let [[y x] from
                                neighbor [(+ y dy) (+ x dx)]
                                neighbor-height (get g neighbor)
                                own-height (g from)]
                          :when (and (= neighbor-height (inc own-height))
                                     (not (get seen_ neighbor)))]
                      neighbor)]
      (->> (mapcat |(find-paths g $ target-value seen_) neighbors)
           (filter (complement empty?))
           (map |(tuple/join [from] $))))))

(defn score [paths] (->> (map last paths) distinct length))

(defn rating [paths] (length (distinct paths)))

(defn solve [input]
  (var p1 0)
  (var p2 0)
  (let [grid (->> (peg/match parser input)
                  (map (fn [[y x n]] [[y x] n]))
                  from-pairs)]
    (loop [[point height] :in (pairs grid)
           :when (= height 0)
           :let [paths (find-paths grid point 9)]]
      (+= p1 (score paths))
      (+= p2 (rating paths)))
    [p1 p2]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (peg/match parser (slurp "d10/ex.txt")))
