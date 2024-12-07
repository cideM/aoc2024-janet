(def parser (peg/compile ~(split :s (any (group (* (line) (column) '1))))))

(defn obstacle? [s] (or (= s "O") (= s "#")))

(defn move [[y x] [dy dx]] [(+ y dy) (+ x dx)])

(defn turn-right [[y x]] [x (* -1 y)])

(defn find-next [grid [pos vec]]
  (->> (map |[(move pos $) $]
            [vec
             (turn-right vec)
             (turn-right (turn-right vec))
             (turn-right (turn-right (turn-right vec)))])
       (find (comp not obstacle? grid first))))

(def arrow-to-vec {"^" [-1 0] ">" [0 1] "v" [1 0] "<" [0 -1]})

(defn find-start [grid]
  (->> (pairs grid)
       (find (fn [[_ arrow]] (arrow-to-vec arrow)))))

(defn run [grid]
  (var g (table/clone grid))
  (let [[start-pos start-arrow] (find-start grid)]
    (var pose [start-pos (arrow-to-vec start-arrow)])
    (var seen @{pose 1})
    (loop [_ :iterate true
           :until (or (> (seen pose) 1)
                      (not (get grid (first pose))))]
      (set pose (find-next grid pose))
      (update seen pose |(cond $ (inc $) 0)))
    {:seen seen :exit (if (> (seen pose) 1) :loop :oob)}))

(defn solve [input]
  (let [grid (->> (string/trimr input)
                  (peg/match parser)
                  (map (fn [[y x v]] [[y x] v]))
                  from-pairs)
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
