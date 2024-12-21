(def parser (peg/compile
              ~(split "\n" (group (* (number :d+) "," (number :d+))))))

(def neighbor-dirs [[-1 0] [0 1] [1 0] [0 -1]])

(defn move [[y x] [dy dx]] [(+ y dy) (+ x dx)])

(defn bfs [max-y max-x obstacles start goal]
  (var found nil)
  (var queue @[{:pos start :path []}])
  (var unvisited (from-pairs (seq [y :range-to [0 max-y]
                                   x :range-to [0 max-x]]
                               [[y x] true])))
  (loop [_ :iterate true :until (or (truthy? found) (empty? queue))
         :after (array/remove queue 0)
         :let [{:pos pos :path path} (first queue)
               neighbors (->> (map |(move pos $) neighbor-dirs)
                              (filter unvisited)
                              (filter (complement obstacles))
                              (map |{:pos $
                                     :path (array/push
                                             (array/slice path) pos)}))]]
    (when (= pos goal) (set found path))
    (each n neighbors (put unvisited (n :pos) nil) (array/push queue n)))
  found)

(defn solve [input]
  (let [bytes (peg/match parser (string/trimr input))
        make-map (fn [n] (->> (take n bytes)
                              (map (fn [[x y]] [[y x] true]))
                              from-pairs))
        max-y 70 max-x 70 start [0 0] goal [max-y max-x]
        path (bfs max-y max-x (make-map 1024) start goal)
        cutoff (find |(nil? (bfs max-y max-x (make-map $) start goal))
                     (range 1 (length bytes)))]
    [(length path) (bytes (dec cutoff))]))

(defn main [&] (->> (file/read stdin :all) solve pp))

(comment
  (peg/match parser (string/trimr (slurp "d18/ex.txt"))))
