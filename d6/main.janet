(def parser (peg/compile ~(split :s (any (group (* (line) (column) '1))))))

(defn obstacle? [s] (or (= s "O") (= s "#")))

(defn move [[y x] [dy dx]] [(+ y dy) (+ x dx)])

(defn turn [[y x]] [x (* -1 y)])

(defn find-next [grid pos startvec]
  (var vec startvec)
  (loop [_ :iterate true :until (->> (move pos vec) grid obstacle? not)]
    (set vec (turn vec)))
  vec)

(defn run [grid]
  (var g (table/clone grid))
  (let [[[start-y start-x] start-dir] (find (fn [[_ v]] (= "^" v)) (pairs grid))]
    (var curpos [start-y start-x])
    (var vec ({"^" [-1 0] ">" [0 1] "v" [1 0] "<" [0 -1]} start-dir))
    (var seen @{[curpos vec] 1})
    (loop [_ :iterate true
           :until (or (> (seen [curpos vec]) 1) (not (get grid curpos)))
           :let [[y x] curpos
                 next-vec (find-next grid curpos vec)
                 next-pos (move curpos next-vec)]]
      (set curpos next-pos)
      (set vec next-vec)
      (update seen [next-pos next-vec] |(cond $ (inc $) 0)))
    {:seen seen :exit (if (> (seen [curpos vec]) 1) :loop :oob)}))

(defn make-grid [parsed-pairs]
  (from-pairs (map (fn [[y x v]] [[y x] v]) parsed-pairs)))

(defn solve [input]
  (let [grid (->> (string/trimr input) (peg/match parser) make-grid)
        {:seen seen} (run grid)
        seen-locations (->> (keys seen)
                            (map (fn [[pos _]] [pos true]))
                            distinct
                            from-pairs)
        # When leaving the grid I count one more step
        p1 (dec (length seen-locations))
        p2 (count
             truthy?
             (seq [[point cell] :in (pairs grid)
                   :when (and (get seen-locations point)
                              (not= cell "^"))]
               (= :loop
                  ((run (merge-into (table/clone grid) {point "O"})) :exit))))]
    [p1 p2]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (solve (slurp "d6/ex.txt"))
  (solve (slurp "d6/in.txt")))
