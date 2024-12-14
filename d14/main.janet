(def parser
  (peg/compile ~{:main (split "\n" (group (* :pos :s :velocity)))
                 :pos (* "p=" :number "," :number)
                 :velocity (* "v=" :number "," :number)
                 :number (number (* (? "-") :d+))}))

(defn print-robots [max-x max-y robots]
  (loop [y :range-to [0 max-y]]
    (print (string ;(seq [x :range-to [0 max-x]] (if (get robots [x y]) "o" " "))))))

(defn from-pairs-merge [pairs]
  (var out @{})
  (loop [[k v] :in pairs]
    (update out k (fn [arr] (array/push (or arr @[]) ;v))))
  out)

(defn wrap-index [len i] (if (neg? i) (+ len (% i len)) (% i len)))

(defn assign-quadrants [max-x max-y robots]
  (var state @[@[] @[] @[] @[]])
  (let [mid-x (dec (math/ceil (/ max-x 2)))
        mid-y (dec (math/ceil (/ max-y 2)))]
    (loop [[[x y] vectors] :pairs robots
           [dx dy] :in vectors
           :when (and (not= x mid-x)
                      (not= y mid-y))]
      (cond
        (and (< x mid-x) (< y mid-y)) (array/push (state 0) [[x y] [dx y]])
        (and (> x mid-x) (< y mid-y)) (array/push (state 1) [[x y] [dx y]])
        (and (< x mid-x) (> y mid-y)) (array/push (state 2) [[x y] [dx y]])
        (and (> x mid-x) (> y mid-y)) (array/push (state 3) [[x y] [dx y]]))))
  state)

(defn turn [max-x max-y robots]
  (var new-robots @{})
  (loop [[[x y] vectors] :pairs robots
         [dx dy] :in vectors
         :let [[x2 y2] [(+ x dx) (+ y dy)]
               x2-wrapped (wrap-index max-x x2)
               y2-wrapped (wrap-index max-y y2)]]
    (update new-robots [x2-wrapped y2-wrapped]
            (fn [arr] (array/push (or arr @[]) [dx dy]))))
  new-robots)

(defn gen-turns [max-x max-y robots]
  (var robots robots)
  (generate [_ :iterate true] (set robots (turn max-x max-y robots))))

(defn longest-run [y max-x robots]
  (var results @[])
  (var is-in false)
  (var cur-count 0)
  (loop [x :range-to [0 max-x]
         :let [occupied (truthy? (get robots [x y]))]]
    (cond
      (and occupied (not is-in)) (do (set cur-count 1) (set is-in true))
      (and occupied is-in) (+= cur-count 1)
      (and (not occupied) is-in) (do (array/push results cur-count)
                                   (set cur-count 0)
                                   (set is-in false))))
  (array/push results cur-count)
  (max ;results))

(defn tree? [max-x max-y robots]
  (find |(>= (longest-run $ max-x robots) 20) (range 0 (inc max-y))))

(defn solve [input]
  (var turns 0)
  (let [robots (->> (string/trimr input)
                    (peg/match parser)
                    (map (fn [[x y dx dy]] [[x y] [[dx dy]]]))
                    from-pairs-merge)
        max-x 101
        max-y 103
        p1 (->> (gen-turns max-x max-y robots) (take 100) last
                (assign-quadrants max-x max-y) (map length) product)
        p2 (find |(do (+= turns 1) (tree? max-x max-y $))
                 (gen-turns max-x max-y robots))]
    (do
      (print p1)
      (print-robots max-x max-y p2)
      (print turns))))

(defn main [&] (->> (file/read stdin :all) solve pp))
