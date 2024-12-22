(def parser (peg/compile ~(split :s (any (group (* (line) (column) '1))))))

(defn dist [[y x] [y2 x2]] (+ (math/abs (- y2 y)) (math/abs (- x2 x))))

(defn bfs [g start]
  (var q @[start])
  (var dists @{start 0})
  (loop [_ :iterate true :until (empty? q) :after (array/remove q 0)
         :let [cur (first q) [y x] cur]]
    (loop [[dy dx] :in [[0 1] [1 0] [0 -1] [-1 0]]
           :let [p [(+ y dy) (+ x dx)]]
           :when (and (not= "#" (get g p)) (not (get dists p)))]
      (array/push q p)
      (put dists p (inc (dists cur)))))
  dists)

(defn shortcuts [g dists min-saved max-steps]
  (let [points (keys dists)
        combos (distinct
                 (catseq [i :range [0 (length points)] j
                          :range [(inc i) (length points)]
                          :when (< (dist (points i) (points j)) max-steps)]
                         [[(points j) (points i)]
                          [(points i) (points j)]]))]
    (sum (seq [[[y x] [y2 x2]] :in combos
               :let [shortcut-dist (dist [y2 x2] [y x])
                     time-saved (- (dists [y2 x2]) (dists [y x]) shortcut-dist)]
               :when (and (>= time-saved min-saved)
                          (< shortcut-dist max-steps))] 1))))

(defn solve [input]
  (let [grid (->> (peg/match parser input)
                  (map (fn [[y x n]] [[y x] n]))
                  from-pairs)
        start (->> (pairs grid) (find |(= (last $) "S")) first)
        dists (bfs grid start)
        p1 (shortcuts grid dists 100 3)
        p2 (shortcuts grid dists 100 21)]
    [p1 p2]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (peg/match parser (slurp "d20/ex.txt")))
