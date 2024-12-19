(def neighbor-dirs [[-1 0] [0 1] [1 0] [0 -1]])

(def move-cost {[0 1] {[0 1] 1 [1 0] 1001 [-1 0] 1001 [0 -1] 2001}
                [0 -1] {[0 -1] 1 [1 0] 1001 [-1 0] 1001 [0 1] 2001}
                [1 0] {[0 -1] 1001 [1 0] 1 [-1 0] 2001 [0 1] 1001}
                [-1 0] {[0 -1] 1001 [1 0] 2001 [-1 0] 1 [0 1] 1001}})

(def parser (peg/compile ~(split :s (any (group (* (line) (column) '1))))))

(defn pop-min
  "Surely there must be a simpler way to get the Lua equivalent of
  `table.remove(t, 1)`"
  [arr dists]
  (var head nil)
  (let [sort-by-dist (fn [a b] (< (dists a) (dists b)))]
    (do
      (sort arr sort-by-dist)
      (set head (first arr))
      (array/remove arr 0)
      head)))

(defn backtrack
  "Starting with `init` [pos dir] pairs, which all achieved the best
  (lowest) score, follow the backlinks to previous nodes, and gather them all
  up. Visit only nodes on the best score path by making sure that the cost
  of the previous node, plus the cost of moving to the next node, equals
  that of the next node. Since the `init` nodes all have the best score,
  this ensures that all visited nodes contributed to the best score."
  [dists prev init]
  (var seen @{})
  (var queue @[;init])
  (loop [_ :iterate true :until (empty? queue)
         :let [cur (first queue)
               [cur-pos cur-dir] cur
               nodes (or (prev cur) @[])
               best? (fn [neighbor]
                       (let [[_ dir] neighbor
                             cost-to-cur (get-in move-cost [dir cur-dir])]
                         (= (dists cur) (+ (dists neighbor) cost-to-cur))))]
         :after (array/remove queue 0)]
    (put seen cur-pos true)
    (array/push queue ;(->> (filter best? nodes))))
  seen)

(defn dijkstra [g start]
  (var queue @[[start [0 1]]])
  (var prev @{})
  (var dists @{})
  (loop [[p _] :pairs g
         dir :in neighbor-dirs] (put dists [p dir] math/inf))
  (put dists [start [0 1]] 0)
  (loop [_ :iterate true :until (empty? queue)
         :let [cur (pop-min queue dists) [cur-pos cur-dir] cur [y x] cur-pos]]
    (loop [[dy dx] :in neighbor-dirs
           :let [neighbor [(+ y dy) (+ x dx)]
                 dirneighbor [neighbor [dy dx]]
                 alt-cost (+ (dists cur)
                             (get-in move-cost [cur-dir [dy dx]]))]
           :when (not= "#" (g neighbor))]
      # <= is important to add *all* backlinks on the best path
      (when (<= alt-cost (dists dirneighbor))
        (update prev dirneighbor (fn [arr] (array/push (or arr @[]) cur))))
      (when (< alt-cost (dists dirneighbor))
        (put dists dirneighbor alt-cost)
        (array/push queue dirneighbor))))
  [dists prev])

(defn solve [input]
  (let [grid-pairs (peg/match parser (string/trimr input))
        start (take 2 (find (fn [[y x v]] (= v "S")) grid-pairs))
        goal (take 2 (find (fn [[y x v]] (= v "E")) grid-pairs))
        grid (->> (map (fn [[y x n]] [[y x] n]) grid-pairs) from-pairs)
        [dists prev] (dijkstra grid start)
        best (min ;(map |(dists [goal $]) neighbor-dirs))
        best-dirs (filter |(= best (dists [goal $])) neighbor-dirs)
        visited (backtrack dists prev (map |[goal $] best-dirs))]
    [best (length visited)]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (->> (slurp "d16/ex.txt") solve)
  (peg/match parser (slurp "d16/ex.txt")))
