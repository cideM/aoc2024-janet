(def parser (peg/compile ~(split :s (any (group (* (line) (column) '1))))))

(defn map* [f ds] (coro (each v ds (yield (f v)))))
(defn filter* [f ds] (coro (each v ds (when (f v) (yield v)))))
(def arrow-to-vec {"^" [-1 0] ">" [0 1] "v" [1 0] "<" [0 -1]})
(defn safe-inc [n] (default n 0) (inc n))
(defn obstacle? [s] (or (= s "O") (= s "#")))
(defn move [[y x] [dy dx]] [(+ y dy) (+ x dx)])
(defn turn-right [[y x]] [x (* -1 y)])

# A 'pose' is a combination of position [y x] and direction [dy dx]

(defn find-valid-pose
  "Find a tile that's not occupied and reachable from the current
  position. Returns the [position direction] to get to that tile."
  [grid [pos vec]]
  (let [vectors [vec
                 (turn-right vec)
                 (turn-right (turn-right vec))
                 (turn-right (turn-right (turn-right vec)))]]
    (->> (map |[(move pos $) $] vectors)
         (find (comp not obstacle? grid first)))))

(defn find-start [grid]
  (->> (pairs grid) (find (fn [[_ arrow]] (arrow-to-vec arrow)))))

(defn run
  "Make the guard patrol the maze until they either leave the maze or arrive
  in the same spot, with the same direction twice. Returns the visited
  [location direction] pairs, and whether the run was finished because of
  a :loop or :oob (out of bounds)."
  [grid]
  (let [[start-pos start-arrow] (find-start grid)]
    (var pose [start-pos (arrow-to-vec start-arrow)])
    (var seen @{pose 1})
    (loop [_ :iterate true
           :until (or (> (seen pose) 1) (->> (first pose) (get grid) not))]
      (set pose (find-valid-pose grid pose))
      (update seen pose safe-inc))
    {:seen seen :exit (if (> (seen pose) 1) :loop :oob)}))

(defn gen-loc-stream [locations] (coro (each loc locations (yield loc))))

(defn solve [input]
  (let [grid (->> (string/trimr input)
                  (peg/match parser)
                  (map (fn [[y x v]] [[y x] v]))
                  from-pairs)
        {:seen seen} (run (table/clone grid))
        visited (->> (keys seen) (map first) distinct)
        p1 (dec (length visited)) # When leaving the grid I count one more step
        p2 (->> (gen-loc-stream visited)
                (filter* |(not= (grid $) "^"))
                (map* |(as-> {$ "O"} _
                             (merge-into (table/clone grid) _)
                             (run _)
                             (_ :exit)))
                (count |(= :loop $)))]
    [p1 p2]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (solve (slurp "d6/ex.txt"))
  (solve (slurp "d6/in.txt")))
