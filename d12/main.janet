(def parser (peg/compile ~(split :s (any (group (* (line) (column) '1))))))

(defn neighbors [[y x]]
  (map (fn [[dy dx]] [(+ y dy) (+ x dx)]) [[-1 0] [0 1] [1 0] [0 -1]]))

(defn make-fences
  "Visits all points (by traversing `membership`) and counts the number of
  fences (points with non-area neighbors) and points per area."
  [grid membership]
  (var area-sizes @{})
  (var area-perimeters @{})
  (loop [[[y x] area] :pairs membership
         :let [fences (->> (neighbors [y x])
                           (count |(not= area (get membership $))))]]
    (do (update area-sizes area |(inc (or $ 0)))
      (update area-perimeters area |(+ (or $ 0) fences))))
  (->> (seq [[area size] :pairs area-sizes]
         [area [size (area-perimeters area)]])
       from-pairs))

(defn sides-by-area [grid membership]
  (var sides @{})
  (var state @{})
  (let [max-y (max ;(->> (pairs grid) (map (fn [[[y x] _]] y))))
        max-x (max ;(->> (pairs grid) (map (fn [[[y x] _]] x))))]
    (loop [y :range-to [1 max-y] x :range-to [1 max-x]
           :let [point [y x] area (membership point)]]

      (each [cur dir prev] [[[(dec y) x] :up [area [y (dec x)] :up]]
                            [[y (inc x)] :right [area [(dec y) x] :right]]
                            [[y (dec x)] :left [area [(dec y) x] :left]]
                            [[(inc y) x] :down [area [y (dec x)] :down]]]
        (when (->> (membership cur) (not= area))
          (put-in state [area point dir] true)
          (when (not (get-in state prev))
            (update sides area (fn [x] (inc (or x 0)))))))))
  sides)

(defn discover-area
  "Uses breadth-first traversal to discover an area by following neighbors
  of the same plant type until none remain."
  [grid [y x]]
  (let [plant (get grid [y x])]
    (var seen @{})
    (var queue @[[y x]])
    (seq [_ :iterate true :while (not (empty? queue))
          :let [cur (first queue)
                next (->> (neighbors cur)
                          (filter |(and (nil? (seen $)) (= plant (grid $)))))]
          :when (= plant (grid cur))
          :before (put seen cur true)
          :after (do (array/remove queue 0)
                   (array/push queue ;next)
                   (each n next (put seen n true)))] cur)))

(defn assign-areas
  "Creates new areas as necessary and discovers them by calling
  `discover-area`on the first point in a new area. Points that are already
  a member of an area are not discovered."
  [grid]
  (var areas @[])
  (var membership @{})
  (loop [[point ch] :pairs grid :when (nil? (get membership point))]
    (let [new-area (inc (length areas))
          members (discover-area grid point)]
      (array/push areas new-area)
      (each p members (put membership p new-area))))
  [areas membership])

(defn solve [input]
  (let [grid (->> (string/trimr input)
                  (peg/match parser)
                  (map (fn [[y x ch]] [[y x] ch]))
                  from-pairs)
        [areas membership] (assign-areas grid)
        fences (make-fences grid membership)
        p1 (sum (->> (pairs fences)
                     (map (fn [[area [size fences]]] [area (* size fences)]))
                     (map last)))
        sides (sides-by-area grid membership)
        p2 (sum (->> (pairs fences)
                     (map (fn [[area [size _]]] [area (* size (sides area))]))
                     (map last)))]
    [p1 p2]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (peg/match parser (slurp "d12/ex.txt")))
