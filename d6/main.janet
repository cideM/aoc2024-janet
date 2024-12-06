(def parser (peg/compile ~(split :s (any (group (* (line) (column) '1))))))

(def neighbor-dir {"^" [-1 0] ">" [0 1] "v" [1 0] "<" [0 -1]})

(defn obstacle? [s] (or (= s "O") (= s "#")))

(defn move [[y x] [dy dx]] [(+ y dy) (+ x dx)])

(defn find-next [grid pos startdir]
  (var dir startdir)
  (loop [_ :iterate true
         :let [next (grid (move pos (neighbor-dir dir)))]
         :until (not (obstacle? next))]
    (set dir
         (cond (= dir "^") ">"
           (= dir ">") "v"
           (= dir "v") "<"
           (= dir "<") "^")))
  dir)

(defn run [grid]
  (var g (table/clone grid))
  (let [[[start-y start-x] start-dir] (find (fn [[_ v]] (neighbor-dir v)) (pairs grid))]
    (var curpos [start-y start-x])
    (var curdir start-dir)
    (var seen @{[curpos curdir] 1})
    (loop [_ :iterate true
           :until (or (> (seen [curpos curdir]) 1)
                      (not (get grid curpos)))
           :let [[y x] curpos
                 next-dir (find-next grid curpos curdir)
                 next-pos (move curpos (neighbor-dir next-dir))]]
      (merge-into g {next-pos next-dir})
      (set curpos next-pos)
      (set curdir next-dir)
      (update seen [next-pos next-dir] (fn [n] (if (truthy? n) (inc n) 0))))
    {:grid g
     :seen seen
     :exit (if (> (seen [curpos curdir]) 1) :loop :oob)}))

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
