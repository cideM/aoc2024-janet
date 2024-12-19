(def parser (peg/compile
              ~{:main (some (* (+ :grid-line :dir-line) (any :s)))
                :dir-line (group (some '(+ ">" "v" "^" "<")))
                :grid-line (group (* (line) (column) '(+ "#" "." "O" "@")))}))

(def vectors {">" [0 1] "^" [-1 0] "v" [1 0] "<" [0 -1]})

(defn move [[y x] [dy dx]] [(+ y dy) (+ x dx)])

(defn bracket? [s] (or (= s "]") (= s "[")))

(defn small-box? [s] (= s "O"))

(def clones {"#" ["#" "#"] "O" ["[" "]"] "." ["." "."] "@" ["@" "."]})

(defn other-bracket [grid pos]
  (let [vec (if (= "[" (grid pos)) [0 1] [0 -1])] (move pos vec)))

(defn scale [grid start]
  (var new-grid @{})
  (var grid-arr @[])
  (let [ps (pairs grid)
        max-y (max ;(->> ps (map (comp first first))))
        max-x (max ;(->> ps (map (comp last first))))]
    (loop [y :range-to [0 max-y]
           :let [row (seq [x :range-to [0 max-x]] (grid [y x]))]]
      (array/push grid-arr row)))
  (let [grid-arr-scaled (map |(mapcat (fn [x] (clones x)) $) grid-arr)]
    (loop [[y row] :pairs grid-arr-scaled [x cell] :pairs row]
      (put new-grid [y x] cell)))
  new-grid)

(defn gen-coords [start vec]
  (var pos start)
  (generate [_ :iterate true]
    (let [next-pos (move pos vec)] (set pos next-pos) pos)))

(defn box-cluster
  "Given the `pos` of a bracket, discover all brackets in the cluster, but
  only counting brackets in `vec` direction. Imagine it like a fan extending
  outwards either up or down."
  [grid pos vec]
  (var seen @{pos true (other-bracket grid pos) true})
  (var queue @[pos (other-bracket grid pos)])
  (generate [_ :iterate true :until (empty? queue)
             :let [cur (first queue) cell (grid cur)]
             :before (array/remove queue 0)
             :after (when (bracket? cell)
                      # [(move into `vec` direction from current bracket (up/down)
                      #  (move to other bracket in current pair (left/right))]
                      (loop [p :in [(move cur vec) (other-bracket grid cur)]
                             :when (not (seen p))]
                        (put seen p true)
                        (array/push queue p)))]
    [cell cur]))

(defn move-big-box [grid pos vec]
  (let [boxes (box-cluster grid (move pos vec) vec)
        valid (take-until |(= "#" (first $)) boxes)]
    # if we can resume it, we hit the `take-until` case, meaning there's a '#'
    (if (fiber/can-resume? boxes)
      pos
      (do
        (loop [[cell p] :in valid :when (bracket? cell)]
          (put grid p "."))
        (loop [[cell p] :in valid :when (bracket? cell)]
          (put grid (move p vec) cell))
        (move pos vec)))))

(defn move-small-box [g pos vec &opt box?]
  (default box? bracket?)
  (if-let [fib (gen-coords pos vec)
           ahead (take-until |(not (box? (g $))) fib)
           can-move (->> (fiber/last-value fib) (get g) (= "."))]
    (do
      (loop [p :in (reverse ahead)] (put g (move p vec) (g p)) (put g p "."))
      (move pos vec))
    pos))

(defn walk [grid pos dir]
  (let [vec (vectors dir) next (move pos vec) cell (grid next)]
    (cond
      (= cell ".") [grid next]
      (small-box? cell) [grid (move-small-box grid pos vec small-box?)]
      (and (bracket? cell)
           (or (= dir "<") (= dir ">"))) [grid (move-small-box grid pos vec)]
      (bracket? cell) [grid (move-big-box grid pos vec)]
      [grid pos])))

(defn gen-grid [grid start dirs walker]
  (var pos start)
  (var g (put (table/clone grid) start "."))
  (coro (each d dirs
          (let [[_ next] (walker g pos d)]
            (set pos next)
            (yield [g pos d])))))

(defn score [g ok?]
  (sum (seq [[p _] :pairs g :when (ok? (g p))] (+ (dec x) (* 100 (dec y))))))

(defn solve [input]
  (let [data (->> (peg/match parser input) (group-by |(= 3 (length $))))
        grid (->> (data true) (map (fn [[y x cell]] [[y x] cell])) from-pairs)
        dirs (->> (data false) flatten)
        @ (->> (pairs grid) (find |(= "@" (last $))) first)
        grid-p2 (scale grid @)
        @-p2 (->> (pairs grid-p2) (find |(= "@" (last $))) first)
        [p1] (->> (gen-grid grid @ dirs walk) (map identity) last)
        [p2] (->> (gen-grid grid-p2 @-p2 dirs walk) (map identity) last)]
    [(score p1 small-box?) (score p2 |(= $ "["))]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (solve (slurp "d15/in.txt")))
